;;;; -*- mode: lisp -*-

(in-package :loom)

;;;; ===========================================================================
;;;; ===========================================================================
;;;;
;;;; Loom objects
;;;; A simple persistence mechanism for storing
;;;; lisp objects in the loom archive
;;;;
;;;; ===========================================================================
;;;; ===========================================================================

;;; 
;;; Global variables
;;; 

(defvar *loom-store* nil
  "The current loom store")

(defvar *loom-reader-package* nil
  "Override (package-of loom-store) during initialization")

(defparameter *loom-package* (find-package :loom)
  "The default loom-store package.")

(defparameter *loom-class-slots*
  '(class-name slots instances-loc))


;;; ============================================================================
;;; 
;;; Loom store classes
;;; 
;;; ============================================================================

;;; 
;;; Class definitions
;;; 

(defclass loom-persist (standard-class)
  ((ignored-slots
    :accessor ignored-slots-of
    :initarg :ignore
    :initform nil
    :type (or list nil)
    :documentation "A list of slot names that will not be persisted.")
   (loom-store
    :accessor loom-store-of
    :initform *loom-store*
    :type (or null loom-store)
    :documentation "The associated loom-store.")
   (id-counter
    :accessor id-counter-of
    :initform 0
    :type integer
    :documentation "A newly allocated instance will receive this id,
and this slot will be incremented")
   (instances->ids
    :accessor instances->ids-of
    :initform (make-weak-hash-table :weakness :key)
    :type hash-table
    :documentation "A weak hashtable mapping instances -> ids")
   (ids->instances
    :accessor ids->instances-of
    :initform (make-weak-hash-table :weakness :value
                                    :test #'eql)
    :type hash-table
    :documentation "A weak hashtable mapping instance ids -> instances"))
  (:documentation
   "A metaclass, classes of which will have themselves, and instances of
themselves be persisted to the current *loom-store*."))

;;; ----------------------------------------------------------------------------

(defclass loom-store ()
  ((server
    :initarg :server
    :accessor server-of
    :type loom-server)
   (usage-loc
    :initarg :usage-loc
    :initform nil
    :accessor usage-loc-of
    :type (or loom-loc null))
   (root-loc
    :initarg :root-loc
    :initform (config-option 'last-root-loc)
    :accessor root-loc-of
    :type (or loom-loc null))
   (untracked-objects-loc
    :initarg :untracked-objects-loc
    :initform nil
    :accessor untracked-objects-loc-of
    :type (or loom-loc null))
   (classes-loc
    :initarg :classes-loc
    :initform nil
    :accessor classes-loc-of
    :type (or loom-loc null))
   (package
    :initarg :package
    :initform *package*
    :accessor package-of
    :type package)
   (untracked-key-value-loc-hash
    :initform (make-hash-table :test 'equal)
    :accessor untracked-key-value-loc-hash
    :documentation "untracked object key -> (value-type value-loc)")
   (class/id->location
    :initform (make-hash-table :test 'equal)
    :accessor id-to-location-hash-of
    :documentation "id -> location")
   (location->class/id
    :initform (make-hash-table :test 'equal)
    :accessor location-to-id-hash-of
    :documentation "location -> id")
   (instances
    :initform (make-hash-table :test 'eq)
    :accessor instances-of
    :documentation "instance -> t")
   (class-hash
    :initform (make-hash-table :test 'eq)
    :accessor class-hash-of
    :documentation "class-name -> class metaobject")
   ))

;;; 
;;; loom-store methods
;;; 

(defmethod print-object ((store loom-store) stream)
  (print-unreadable-object (store stream :type t)
    (format stream "~s ~s"
            (base-uri-of (server-of store))
            (root-loc-of store))))

;;; ============================================================================
;;; 
;;; Persistence reader/writer definitions
;;; 
;;; ============================================================================

;;; 
;;; Reader/writer function hashes
;;; 

(defvar *loom-value-writers*
  (make-hash-table :test 'eq)
  "Map type symbols to functions taking an object of that type as a single
argument, and returning a string.")

(defvar *loom-value-readers*
  (make-hash-table :test 'eq)
  "Map type symbols to functions taking a string and returning an object
of the keyed type.")

;;; 
;;; Storage struct definitions
;;; 

(defstruct %loom-root
  "Defines the structure used to serialize the root node of the store.
Contains references to the %loom-node storing class locations, the usage-token
location, and the package used for the lisp reader. The untracked objects
slot links a %loom-object which links objects that are stored without
MOP trickery."
  (class-loc nil :type (or string null))
  (untracked-objects-loc nil :type (or string null))
  (usage-loc nil :type (or string null))
  (package (package-name *package*) :type (or string null)))

;;; ----------------------------------------------------------------------------

(defparameter *valid-loom-node-types*
  '(class-list instance-list))

(defstruct %loom-node
  "Defines the structure used to serialize interior nodes, such as the list of 
class/location pairs referenced by the %loom-root, and the list of instance-id/
instance-location pairs stored at each class's location."
  (type nil :type (or symbol null))
  (next nil :type (or string null))
  (elements nil :type (or cons null)))

;;; ----------------------------------------------------------------------------

(defstruct %loom-untracked-object
  "Defines a struct used to store raw key/value pairs. 'Pairs' is list of quads
defining (type key type value). The each key/value is a reference to a loom-loc
that is [de]serialized by the methods [read-from/write-to]-loom-store method.
When value-type is 'gc, and value is nil, the object may be garbage collected."
  (next nil :type (or string null))
  (pairs nil :type (or cons null)))

;;; ----------------------------------------------------------------------------

(defstruct %loom-object
  "Defines the structure used to serialize object instances within the store.
'Class' stores the symbol-name of this object's class, and 'slots' stores
a list of slot-name/slot-type/slot-location triples (symbol symbol string).
The function hashed at slot-type from *loom-value-[readers/writers]* will be
used to read/write the leaf and return a lisp object."
  (class nil :type (or symbol null))
  (next nil :type (or string null))
  (slots nil :type (or cons null)))

;;; ----------------------------------------------------------------------------

(defstruct %loom-indirection
  "Defines a reference to another loom object. A nil ref represents an unbound
reference."
  (type nil :type (or symbol null))
  (ref nil :type (or string null)))

;;; 
;;; Raw Loom accessor functions
;;; 

(defun %loom-store-get (loc package)
  (with-standard-io-syntax
    (let ((*package* package))
      (read-from-string (archive-touch loc)))))

;;; ----------------------------------------------------------------------------

(defun loom-store-get (loc)
  "Returns a lisp object stored at loc with standard read syntax.
Uses the values from *loom-store*"
  (with-loom-store (*loom-store*)
    (%loom-store-get loc (package-of *loom-store*))))

;;; ----------------------------------------------------------------------------

(defun (setf %loom-store-get) (value loc package usage)
  "Write a lisp object 'value' to loc with prin1. Create loc if necessary."
  ;; buy a new location if we don't have one
  (check-type loc (or null loom-loc))
  (when (eq loc nil)
    (setf loc (random-vacant-archive-loc))
    (archive-buy loc usage))
  (archive-write loc
                 (with-standard-io-syntax
                   (let ((*package* package))
                     (with-output-to-string (str)
                       (prin1 value str))))
                 usage)
  (values value loc))

;;; ----------------------------------------------------------------------------

(defun (setf loom-store-get) (value &optional loc)
  "Write a lisp object 'value' to loc with prin1. Create loc if necessary.
Uses the values from *loom-store*"
  (with-loom-store (*loom-store*)
    (setf (%loom-store-get loc
                           (package-of *loom-store*)
                           (usage-loc-of *loom-store*))
          value)))

;;; ----------------------------------------------------------------------------

(defun loom-store-sell (location)
  (handler-case (progn
                  (archive-write location "" (usage-loc-of *loom-store*))
                  (archive-sell location (usage-loc-of *loom-store*) t)
                  t)
    (loom-client-error () nil)))

;;; ============================================================================
;;; 
;;; Basic generic loom node manipulators
;;; 
;;; ============================================================================

(defun linked-node-create (node &optional elements)
  "Creates a new linked node of the same type/class as node"
  (etypecase node
    (%loom-node
     (make-%loom-node
      :type (%loom-node-type node)
      :elements elements))
    (%loom-untracked-object
     (make-%loom-untracked-object
      :pairs elements))
    (%loom-object
     (make-%loom-object 
      :class (%loom-object-class node)
      :slots elements))))

;;; ----------------------------------------------------------------------------

(defun linked-node-next (node)
  (slot-value node 'next))

(defun (setf linked-node-next) (value node)
  (setf (slot-value node 'next) value))

;;; ----------------------------------------------------------------------------

(defun linked-node-elements (node)
  (slot-value node
              (etypecase node
                (%loom-node 'elements)
                (%loom-untracked-object 'pairs)
                (%loom-object 'slots))))

(defun (setf linked-node-elements) (value node)
  (setf (slot-value node
                    (etypecase node
                      (%loom-node 'elements)
                      (%loom-untracked-object 'pairs)
                      (%loom-object 'slots)))
        value))

;;; ----------------------------------------------------------------------------

(defun map-linked-nodes (fn location)
  "Maps a function of (location node) over the linked nodes."
  (let* ((current (and (typep location 'loom-loc)
                       (loom-store-get location)))
         (next (and current (linked-node-next current))))
    (when current
      (funcall fn location current)
      (when (typep next 'loom-loc)
        (map-linked-nodes fn next)))))

;;; ----------------------------------------------------------------------------

(defun collect-linked-nodes (location)
  (let (collect)
    (map-linked-nodes (lambda (location current)
                        (declare (ignore location))
                        (push current collect))
                      location)
    (nreverse collect)))

;;; ----------------------------------------------------------------------------

(defmacro do-linked-nodes ((location) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,location) ,@body))
       (declare (dynamic-extent #',thunk))
       (map-linked-nodes #',thunk ,location))))

;;; ============================================================================
;;; 
;;; Loom store creation and modification
;;; 
;;; ============================================================================

(defparameter *root-location-file*
  (merge-pathnames
   (make-pathname :name "root-location"
                  :type "sexp")
   (module-manager:get-directory :cl-loom :up)))

;;; ----------------------------------------------------------------------------

(defun save-root-location (loc &optional (file *root-location-file*))
  (with-open-file (stream file
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (let ((*print-pretty* t)
          (*print-case* :downcase))
      (pprint-linear stream (list 'root-location loc) t))))

;;; ----------------------------------------------------------------------------

(defun load-root-location (&optional (file *root-location-file*))
  (with-standard-io-syntax
    (cadr (with-open-file (str file :direction :input :if-does-not-exist nil)
           (or (and str (read str nil nil))
               (error (format nil "Failed to read ~s" file)))))))

;;; ----------------------------------------------------------------------------

(defun make-loom-store (&key
                        (root-location-load-path *root-location-file*)
                        save-location
                        (server *loom-server*)
                        root-loc
                        usage-loc
                        (package *package*))
  "Loads a previously defined loom store from a root location in a loom server, 
or creates a new one. Set root-loc to t to load a root-loc from
root-location-load-path, to nil to create a new loom-store, or to a loom-loc to
load a specified root loc. When save-location is true, the location of the loaded
loom-store will be written to the root-location-load-path. Package defines the
reader package for loom-store serialization. Binds the resulting loom-store to
*loom-store*."
  (check-type server loom-server)
  (check-type root-location-load-path (or string pathname))
  (when (and (or (typep root-location-load-path 'string)
                 (typep root-location-load-path 'pathname))
             (eq root-loc t))
    (setf root-loc (load-root-location root-location-load-path)))
  (check-type root-loc (or loom-loc null))
  (let* ((package-name (package-name package))
         (usage-loc (or usage-loc
                        (error "No usage location defined")))
         (root-node
          (or (when root-loc (loom-store-get root-loc))
              (make-%loom-root
               :class-loc
               (nth-value 1
                          (setf (%loom-store-get nil package-name usage-loc)
                                (make-%loom-node :type 'class-list)))
               :untracked-objects-loc
               (nth-value 1
                          (setf (%loom-store-get nil package-name usage-loc)
                                (make-%loom-untracked-object)))
               :usage-loc usage-loc
               :package package-name))))
    (multiple-value-bind (value loc)
        (setf (%loom-store-get root-loc
                               (%loom-root-package root-node)
                               (%loom-root-usage-loc root-node))
              root-node)
      (declare (ignore value))
      (when save-location (save-root-location loc root-location-load-path))
      (make-instance 'loom-store
                     :server server
                     :root-loc loc
                     :classes-loc (%loom-root-class-loc root-node)
                     :untracked-objects-loc (%loom-root-untracked-objects-loc
                                             root-node)
                     :usage-loc (%loom-root-usage-loc root-node)
                     :package (find-package (%loom-root-package root-node))))))
  
;;; ----------------------------------------------------------------------------

(defmacro with-loom-store ((loom-store) &body body)
  "Binds *LOOM-STORE* to LOOM-STORE and executes BODY with a transaction
on LOOM-STORE's server."
  `(let ((*loom-store* ,loom-store))
     (with-loom-transaction (:server (server-of *loom-store*))
       ,@body)))

;;; ----------------------------------------------------------------------------

(defun %read-class-instances (store instance-loc)
  (let* ((instance-nodes (collect-linked-nodes
                          instance-loc))
         (elements (mapcan #'%loom-node-elements instance-nodes)))
    (loop
       for id/location-pair in elements
       for id = (car id/location-pair)
       for location = (cadr id/location-pair)
       do
         (setf (gethash id (id-to-location-hash-of store)) location))))

;;; ----------------------------------------------------------------------------

(defun %read-loom-store-classes (store)
  (let* ((root-node (loom-store-get (root-loc-of store)))
         (class-nodes (collect-linked-nodes
                       (%loom-root-class-loc root-node)))
         (elements (mapcan #'%loom-node-elements class-nodes)))
    (loop
       for class/location-pair in elements
       for class = (car class/location-pair)
       for location = (cadr class/location-pair)
       do
         (setf (gethash class (class-hash-of store))
               (find-class class))
         (%read-class-instances store location))))

;;; ----------------------------------------------------------------------------

(defun %read-loom-store-untracked-objects (store)
  (let* ((root-node (loom-store-get (root-loc-of store)))
         (untracked-nodes (collect-linked-nodes
                           (%loom-root-untracked-objects-loc root-node)))
         (elements (mapcan #'%loom-untracked-object-pairs untracked-nodes)))
    (loop
       for k/v-quad in elements
       for key-type = (elt k/v-quad 0)
       for key-object-loc = (elt k/v-quad 1)
       for value-object-loc = (elt k/v-quad 3)
       for key-object = (read-from-location key-type key-object-loc)
       do
         (setf (gethash key-object (untracked-key-value-loc-hash store))
               value-object-loc))))

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :after ((store loom-store)
                                       &rest args)
  (declare (ignore args))
  (with-loom-store (store)
    (%read-loom-store-classes store)
    (%read-loom-store-untracked-objects store)))

;;;
;;; Manipulators
;;;

(defparameter *max-linked-node-length* 100
  "Defines the maximum length of a linked node.")

(defparameter *minimum-linked-node-fill* .75
  "A number ∈ (0.0 1.0], that defines the smallest allowed capacity for a
linked node that is not the last.")

;;; ----------------------------------------------------------------------------

(defun find-in-linked-node (location object
                            &optional (test #'eql) (key #'identity))
  (let ((objects (mapcan #'linked-node-elements
                         (collect-linked-nodes location))))
    (find object objects :test test :key key)))

;;; ----------------------------------------------------------------------------

(defun %append-new-nodes (node objects)
  "Append new linked nodes to hold objects."
  (let ((insert-size (length objects))
        location first-location)
    (loop
       while (plusp insert-size)
       for use = (min *max-linked-node-length* insert-size)
       for nval = (linked-node-create node (subseq objects 0 use))
       do
         (decf insert-size use)
         (setf objects (subseq objects use))
         (multiple-value-bind (nval nloc)
             (setf (loom-store-get) nval)
           (unless first-location
             (setf first-location nloc))
           (when (and node location)
             (setf (linked-node-next node) nloc
                   (loom-store-get location) node))
           (setf node nval
                 location nloc)))
    ;; return the first allocated location
    ;; this is necessary for starting a new linked node list
    first-location))

;;; ----------------------------------------------------------------------------

(defun %insert-into-old-nodes (location objects)
  "Insert objects into empty spaces within the linked node list at location.
Returns the last seen node, location, and the remaining objects."
  (let ((insert-size (length objects))
        last-node last-location)
    (block insert-into
      (map-linked-nodes
       (lambda (location node)
         (let* ((node-elements (linked-node-elements node))
                (use (min insert-size
                          (- *max-linked-node-length*
                             (length node-elements)))))
           (setf last-node node
                 last-location location)
           (when (plusp use)
             (decf insert-size use)
             (setf (linked-node-elements node)
                   (append node-elements (subseq objects 0 use))
                   objects (subseq objects use)
                   (loom-store-get location) node)))
         ;; if all of our inserts are done, return
         (when (zerop insert-size)
           (return-from %insert-into-old-nodes 
             (values nil nil nil))))
       location))
    (values last-node last-location objects)))

;;; ----------------------------------------------------------------------------

(defun add-to-linked-node (objects location)
  "Inserts the object[s] into the linked nodes, specializing on the node type.
When location is nil, returns the location of a new linked node list holding
objects."
  (multiple-value-bind (last-node last-location objects)
      (%insert-into-old-nodes location objects)
    (when (and last-node objects)
      (setf (linked-node-next last-node)
            (%append-new-nodes last-node objects))
      (setf (loom-store-get last-location) last-node))
    location))
               
;;; ----------------------------------------------------------------------------

(defun %linked-node-repack (triples &optional force)
  (labels ((lnm-loc (x) (car x))
           (lnm-node (x) (cadr x))
           (lnm-mod (x) (caddr x))
           (repack-needed? (lnm)
             (loop for el in lnm
                do (when (and (linked-node-next (lnm-node el))
                              (< (/ (length (linked-node-elements
                                             (lnm-node el)))
                                    *max-linked-node-length*)
                                 *minimum-linked-node-fill*))
                     (return-from repack-needed? t)))
             nil)
           (chunk (list chunk)
             (let ((pos 0) (len (length list))
                   col)
               (loop while (< pos len)
                  for use = (min chunk (- len pos))
                  do
                    (push (subseq list pos (+ pos use)) col)
                    (incf pos use))
               (nreverse col))))
    (cond ((or force (repack-needed? triples))
           (let* ((locations (nreverse (mapcar #'lnm-loc triples)))
                  (nodes (nconc (mapcar #'lnm-node triples)
                                (let (col) (map-linked-nodes
                                            (lambda (location node)
                                              (push location locations)
                                              (push node col))
                                            (linked-node-next
                                             (lnm-node (lastcar triples))))
                                     (nreverse col))))
                  (elements (mapcan #'linked-node-elements nodes))
                  (chunks (chunk elements *max-linked-node-length*)))
             (setf locations (nreverse locations))
             (mapc (lambda (location)
                     (loom-store-sell location))
                   (subseq (cdr locations) (max 0 (1- (length chunks)))))
             (mapcar ;; collect location/node/modified triples
              (lambda (location node)
                (list location node t))
              locations
              (mapcar ;; collect packed nodes
               (lambda (location node chunk)
                 (setf (linked-node-elements node) chunk
                       (linked-node-next node) location)
                 node)
               (nconc (cdr (subseq locations 0 (length chunks)))
                      (list nil))
               nodes (or chunks '(nil))))))
          (t triples))))

;;; ----------------------------------------------------------------------------

(defun %linked-node-commit (loc/node/mod-triples)
  (mapc (lambda (x)
          (let ((location (car x))
                (node (cadr x))
                (modified (caddr x)))
            (when modified
              (setf (loom-store-get location) node))))
        loc/node/mod-triples))

;;; ----------------------------------------------------------------------------

(defun remove-from-linked-node (predicate location
                                &key count)
  "Remove elements from the linked nodes at location where predicate evaluates
to t when called on the element. If count is a number, terminate after the first
count deletions. Nodes before the last node are guaranteed to have ≥ to
(* *minimum-linked-node-fill* *max-linked-node-length*) elements."
  (let ((deleted-elements nil)
        (deletions 0)
        (loc/node/modified-triples nil))
    (block deletion-section
      (map-linked-nodes
       (lambda (location node)
         (let (kept-elements del)
           (mapc (lambda (el)
                   (cond ((and (or (not count) (< deletions count))
                               (funcall predicate el))
                          (setf del t)
                          (push el deleted-elements)
                          (incf deletions))
                         (t (push el kept-elements))))
                 (linked-node-elements node))
           (setf (linked-node-elements node) (nreverse kept-elements))
           (push (list location node del) loc/node/modified-triples))
         (when (and count (= deletions count))
           (return-from deletion-section nil)))
       location))
    (let* ((triples (nreverse loc/node/modified-triples))
           (repack (%linked-node-repack triples)))
      (%linked-node-commit repack)
      (values deleted-elements triples))))

;;; ============================================================================
;;; 
;;; Persistant Object Definition
;;; 
;;; ============================================================================

;;;
;;; Meta-object protocol specialization
;;;

(defmethod validate-superclass ((a loom-persist) (b standard-class))
  t)

;;; ----------------------------------------------------------------------------

;; This redefines #'slot-value to use the correct MOP compatible accessor

#.(let ((defun `((defun slot-value (object slot-name)
                   (let ((class (class-of object)))
                     (slot-value-using-class
                      class object
                      (find slot-name (class-slots class)
                            :test #'eq :key #'slot-definition-name))))
                 (defun (setf slot-value) (value object slot-name)
                   (let ((class (class-of object)))
                     (setf (slot-value-using-class
                            class object
                            (find slot-name (class-slots class)
                                  :test #'eq :key #'slot-definition-name))
                           value))))))
    (cond ((or #+ccl t)
           `(let ((,(find-symbol "*WARN-IF-REDEFINE-KERNEL*" :ccl) nil))
              ,@defun))
          ((or #+sbcl t)
           `(,(find-symbol "WITHOUT-PACKAGE-LOCKS" :sb-ext)
                ,@defun))))
            
;;; ----------------------------------------------------------------------------

(defun ensure-loom-class (class)
  (assert (typep class 'loom-persist))
  (with-loom-store (*loom-store*)
    (let* ((name (class-name class))
           (ch (class-hash-of *loom-store*))
           (hash (gethash name ch)))
      (cond (hash hash)
            (t 
             (setf (gethash name ch) class
                   (loom-store-of class) *loom-store*)
             class)))))
                   
#|
(defun persist-loom-instsance (class instance)
  (assert (and (typep instance class)
               (typep class 'loom-persist)))
  (

(defun add-specialized-initializer-to-loom-class (class)
  (let ((lambda (
|#
;;; ----------------------------------------------------------------------------

;(defmethod finalize-inheritance :after ((class loom-persist))
;  (let ((

;;; ----------------------------------------------------------------------------

;(defmethod ensure-class-using-class (


;;; ============================================================================
;;; 
;;; Loom store reader/writer methods
;;; 
;;; ============================================================================

;;;
;;; Reader methods
;;;

(defvar *loom-store-read-string* nil
  "A special variable bound to the raw string returned by loom.")

;;; ----------------------------------------------------------------------------

(defgeneric read-from-location (type location)
  (:argument-precedence-order location type)
  (:documentation "Reads an object of 'type' from the location.
*loom-store-read-string* is available. Methods should eql-specialize on type."))

;;; ----------------------------------------------------------------------------

(defmethod read-from-location (type location)
  (declare (ignore type location))
  nil)

;;;
;;; Writer methods
;;;

(defgeneric write-to-location (object location)
  (:argument-precedence-order location object)
  (:documentation "Writes an object to the location, with updates to
*loom-store*. Users should specialize on object, and return a string."))

;;; ----------------------------------------------------------------------------

(defmethod write-to-location :before (object (location (eql t)))
  (declare (ignore object))
  (archive-buy (random-vacant-archive-loc) (usage-loc-of *loom-store*))
  (call-next-method))

;;; ----------------------------------------------------------------------------

(defmethod write-to-location :around (object location)
  (let ((string (call-next-method object location)))
    (when (stringp string)
      (with-loom-store (*loom-store*)
        (archive-write location string (usage-loc-of *loom-store*))
        (values location string)))))

;;; ----------------------------------------------------------------------------

(defmethod write-to-location (object location)
  (declare (ignore location))
  (error "Cannot write: unknown type ~A [~A]" (type-of object) object))

;;; ----------------------------------------------------------------------------

(defmethod write-to-location ((obj string) location)
  (declare (ignore location))
  obj)

(defmethod write-to-location ((obj number) location)
  (declare (ignore location))
  (with-output-to-string (s)
    (prin1 obj s)))

(defmethod write-to-location ((obj cons) location)
  (declare (ignore location))
  (with-output-to-string (s)
    ))

;; Eventually, handle circular lists here
(defmethod write-to-loom-store ((object cons) stream)
  (write-char #\( stream)
  (loop with first-p = t
     for tail on object
     do
       (if first-p
           (setf first-p nil)
           (write-char #\  stream))
       (write-to-loom-store (car tail) stream)
       (when (and (cdr tail) (atom (cdr tail)))
         (write-string " . " stream)
         (write-to-loom-store (cdr tail) stream)))
  (write-char #\) stream)
  object)

(defmethod write-to-loom-store ((object string) stream)
  (prin1 object stream))

(defmethod write-to-loom-store ((object vector) stream)
  (write-string "#(" stream)
  (loop with first-p = t
     for elt across object
     do
       (if first-p
           (setf first-p nil)
           (write-char #\  stream))
       (write-to-loom-store elt stream))
  (write-char #\) stream)
  object)

(defmethod write-to-loom-store ((object standard-object) stream)
  (check-type *loom-store* loom-store)
  (let* ((store *loom-store*)
         (class (class-name (class-of object)))
         (loc (gethash object (instance-to-loc-hash-of store))))
    (unless loc
      (setf loc (%loom-persist-standard-object store object t)))
    (write-string "#GR(" stream)
    (prin1 class stream)
    (write-char #\space stream)
    (prin1 loc stream)
    (write-char #\) stream)
    object))

#+no
(defun loom-persist-standard-object (object &optional
                                     (loom-store *loom-store*))
  "Write OBJECT to LOOM-STORE.
Updates an existing object's slots in the store.
You must call this to persist all slot value changes.
Returns OBJECT."
  (check-type loom-store loom-store)
  (check-type object standard-object)
  (with-loom-store (loom-store)
    (%loom-persist-standard-object loom-store object nil)))

#+no
(defun %loom-persist-class (store class)
  (unless (gethash (class-name class) (class-hash-of store))
    (let )))

#+no
(defun %loom-persist-standard-object (store object not-present-p)
  (let* ((class-name (class-name (class-of object)))
         (class-hash (class-hash-of store))
         (loom-class (gethash class-name class-hash))
         (loc (unless not-present-p
                (gethash object (instance-to-loc-hash-of store))))
         (usage-loc (usage-loc-of store)))
    (unless loom-class
      (let ((class-loc (add-to-linked-node store (classes-loc-of store)))
            (instances-loc (random-vacant-archive-loc))
            (node `((,class-name nil . nil))))
        (archive-buy instances-loc usage-loc)
        (setf (loom-store-get instances-loc usage-loc) node)
        (setf loom-class (make-loom-class
                          class-name
                          (loom-instance-persisted-slots object)
                          instances-loc))
        (setf (gethash class-name class-hash) loom-class
              (gethash loom-class (instance-to-loc-hash-of store)) class-loc
              (gethash class-loc (loc-to-instance-hash-of store)) loom-class)
        (let ((done nil))
          (unwind-protect
               (progn (%loom-persist-standard-object store loom-class nil)
                      (setf done t))
            (unless done
              ;; Had to add to hashes to make the persisting write
              ;; in the right place.
              ;; Remove now since the persisting failed.
              (remhash class-name class-hash)
              (remhash loom-class (instance-to-loc-hash-of store))
              (remhash class-loc (loc-to-instance-hash-of store)))))))
    (unless loc
      (setf loc (add-to-linked-node store (instances-loc-of loom-class))
            (gethash loc (loc-to-instance-hash-of store)) object
            (gethash object (instance-to-loc-hash-of store)) loc))
    (let ((done nil))
      (unwind-protect
           (let ((str (with-output-to-string (s)
                        (write-string "#GI(" s)
                        (prin1 class-name s)
                        (dolist (slot (slots-of loom-class))
                          (write-char #\space s)
                          (write-to-loom-store
                           (if (slot-boundp object slot)
                               (slot-value object slot)
                               *unbound-marker*)
                           s))
                        (write-char #\) s))))
             (archive-write loc str usage-loc)
             (setf done t))
        (unless done
          ;; Needed to set these hash table values before storing
          ;; so that circular references would work.
          ;; Since we failed to complete the write, remove them now.
          (remhash loc (loc-to-instance-hash-of store))
          (remhash object (instance-to-loc-hash-of store))))))
  object)

#+no
(defun loom-instance-persisted-slots (instance)
  (let* ((class (class-of instance))
         (omitted-slots (loom-instance-omitted-slots instance)))
    (loop for slot in  (closer-mop:class-slots class)
       for slot-name = (closer-mop:slot-definition-name slot)
       when (and (eq (closer-mop:slot-definition-allocation slot) :instance)
                 (not (member slot-name omitted-slots :test #'eq)))
       collect slot-name)))

;; Specialize this to omit slots form your classes
#+no
(defgeneric loom-instance-omitted-slots (instance)
  (:method ((instance t))
    nil)
  (:documentation "Returns a list of the names of slots of instance
to omit from the persistent store.
Specializations should usually cons onto the result of (CALL-NEXT-METHOD)."))

#+no
(defun map-loom-instances-of-class (function class-or-name &optional
                                    (loom-store *loom-store*))
  "Call function repeatedly, with a single argument, each instance
of CLASS-OR-NAME that is in LOOM-STORE."
  (check-type loom-store loom-store)
  (check-type class-or-name (or symbol class))
  (let* ((class-name (if (symbolp class-or-name)
                         class-or-name
                         (class-name class-or-name)))
         (class (if (symbolp class-or-name)
                    (find-class class-or-name)
                    class-or-name))
         (loom-class (gethash class-name (class-hash-of loom-store)))
         (loc-to-instance-hash (loc-to-instance-hash-of loom-store)))
    (when loom-class
      (with-loom-store (loom-store)
        (do-linked-nodes (loc (instances-loc-of loom-class) class-name)
          (let ((instance
                 (or (gethash loc loc-to-instance-hash)
                     (instantiating-class (*instantiating-instance* class loc)
                       (loom-store-get loc)))))
            (funcall function instance)))))))

#+no
(defmacro do-loom-instances-of-class ((instance-var class-or-name &optional
                                                    loom-store)
                                      &body body)
  "Executes BODY repeatedly, with INSTANCE-VAR bound to each instance
of CLASS-OR-NAME in LOOM-STORE."
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,instance-var) ,@body))
       (declare (dynamic-extent #',thunk))
       (map-loom-instances-of-class #',thunk ,class-or-name
                                    ,@(and loom-store (list loom-store))))))

#+no
(defun loom-instances-of-class (class-or-name &optional
                                (loom-store *loom-store*))
  "Returns a list of all instances of CLASS-OR-NAME in LOOM-STORE."
  (let ((res nil))
    (do-loom-instances-of-class (instance class-or-name loom-store)
      (push instance res))
    (nreverse res)))

#+no
(defun wallet-loom-store (location name &key is-passphrase-p usage package)
  "Read or create a loom-store with location of NAME in the wallet
at LOCATION. If IS-PASSPHRASE-P is true, LOCATION is a passphrase.
USAGE is the source of usage tokens for the store. Use the wallet,
if NIL. PACKAGE is the package to use for printing strings in the store.
Use *PACKAGE* if NIL.
Returns a LOOM-STORE instance."
  (with-loom-transaction ()
    (let* ((location (passphrase-location location is-passphrase-p))
           (wallet (get-wallet location))
           (name-location (find-location name wallet))
           (root-loc (and name-location (location-loc name-location))))
      (cond (root-loc (make-loom-store :root-loc root-loc
                                       :usage-loc usage
                                       :package package))
            (t (let ((store (make-loom-store :usage-loc (or usage location)
                                             :package package)))
                 (push (make-location :name name
                                      :loc (root-loc-of store))
                       (wallet-locations wallet))
                 (setf (get-wallet location nil (or usage location))
                       wallet)
                 store))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright 2011 TSC AG, Postfach 73, CH 6314 Unterageri, Switzerland
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions
;;; and limitations under the License.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
