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

(defvar *without-persisting-access* nil
  "Set to t during internal routines to prevent triggering recursive
persistence")

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

   ;; Track low level objects
   (location->untracked
    :initform (make-hash-table :test 'equal)
    :accessor location->untracked-of
    :documentation "location -> untracked object instance")
   (untracked->location
    :initform (make-hash-table :test 'eq)
    :accessor untracked->location-of
    :documentation "untracked object instance -> location")
   (untracked-dependencies
    :initform (make-hash-table :test #'equal)
    :accessor untracked-dependencies-of
    :documentation "object location -> dependency list")

   ;; Track loom-persist objects
   (class/id->location
    :initform (make-hash-table :test 'equal)
    :accessor class/id->location-of
    :documentation "(classname id) -> location")
   (location->class/id
    :initform (make-hash-table :test 'equal)
    :accessor location->class/id-of
    :documentation "location -> (classname id)")
   (instances
    :initform (make-hash-table :test 'eq)
    :accessor instances-of
    :documentation "instance -> t")

   ;; Track classes associated with this store
   (class-hash
    :initform (make-hash-table :test 'eq)
    :accessor class-hash-of
    :documentation "class-object -> location")

   ;; Cache nodes.
   (node-hash
    :initform (make-hash-table :test #'equal)
    :accessor node-hash-of
    :documentation "location -> node struct instance")
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
  "Defines a struct used to store raw objects. Pairs are of the form
[type location]. Objects stored here will be automatically created and 
collected when they are no longer used."
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

;;; 
;;; Raw Loom accessor functions
;;; 

(defmacro with-loom-store ((loom-store) &body body)
  "Binds *LOOM-STORE* to LOOM-STORE and executes BODY with a transaction
on LOOM-STORE's server."
  `(let ((*loom-store* ,loom-store))
     (with-loom-transaction (:server (server-of *loom-store*))
       ,@body)))

;;; ----------------------------------------------------------------------------

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

(defun %instantiate-archive-location (usage &optional loc)
  (let ((location (or (when (typep loc 'loom-loc) loc)
                      (random-vacant-archive-loc))))
    (archive-buy location usage)
    location))

;;; ----------------------------------------------------------------------------

(defun instantiate-archive-location (&optional loc)
  "Create a new archive location at (or loc (random-vacant-archive-loc))"
  (with-loom-store (*loom-store*)
    (%instantiate-archive-location (usage-loc-of *loom-store*) loc)))

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

(defun loom-node-get (loc)
  (or (gethash loc (node-hash-of *loom-store*))
      (setf (gethash loc (node-hash-of *loom-store*))
            (loom-store-get loc))))

(defun (setf loom-node-get) (node &optional loc)
  (cond ((null node)
         (remhash loc (node-hash-of *loom-store*))
         (values nil loc))
        (t (multiple-value-bind (val loc)
               (setf (loom-store-get loc) node)
             (setf (gethash loc (node-hash-of *loom-store*)) val)
             (values val loc)))))

;;; ----------------------------------------------------------------------------

(defun map-linked-nodes (fn location)
  "Maps a function of (location node) over the linked nodes."
  (loop for loc = location then (linked-node-next current)
     for current = (and (typep loc 'loom-loc)
                        (loom-node-get loc))
     while current do
       (funcall fn loc current)))

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
   cl-user::*cl-loom-source-file*))

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
             (probe-file root-location-load-path)
             (eq root-loc t))
    (setf root-loc (load-root-location root-location-load-path)))
  (check-type root-loc (or loom-loc null))
  (let ((root-node nil))
    (cond (root-loc
           (setf root-node (%loom-store-get root-loc package))
           (check-type root-node %loom-root))
          (t (let* ((package-name (package-name package)))
               (check-type usage-loc loom-loc)
               (setf root-node
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
                      :package package-name)))
             (multiple-value-bind (value loc)
                 (setf (%loom-store-get root-loc
                                        (%loom-root-package root-node)
                                        (%loom-root-usage-loc root-node))
                       root-node)
               (declare (ignore value))
               (setf root-loc loc))))
    (when save-location (save-root-location root-loc root-location-load-path))
    (make-instance 'loom-store
                   :server server
                   :root-loc root-loc
                   :classes-loc (%loom-root-class-loc root-node)
                   :untracked-objects-loc (%loom-root-untracked-objects-loc
                                           root-node)
                   :usage-loc (%loom-root-usage-loc root-node)
                   :package (find-package (%loom-root-package root-node)))))

;;; ----------------------------------------------------------------------------

(defun %read-class-instances (store class instance-loc)
  (let* ((instance-nodes (collect-linked-nodes
                          instance-loc))
         (elements (mapcan #'%loom-node-elements instance-nodes))
         (class-name (class-name class))
         (max-id 0))
    (loop
       for id/location-pair in elements
       for id = (car id/location-pair)
       for location = (cadr id/location-pair)
       for pair = (list class-name id)
       do
         (setf (gethash pair (class/id->location-of store)) location
               (gethash location (location->class/id-of store)) pair
               max-id (max max-id id)))
    (clrhash (ids->instances-of class))
    (clrhash (instances->ids-of class))
    (setf (id-counter-of class) (1+ max-id))))

;;; ----------------------------------------------------------------------------

(defun %read-loom-store-classes (store)
  (let* ((root-node (loom-node-get (root-loc-of store)))
         (class-nodes (collect-linked-nodes
                       (%loom-root-class-loc root-node)))
         (elements (mapcan #'%loom-node-elements class-nodes)))
    (loop
       for class/location-pair in elements
       for class-name = (car class/location-pair)
       for class = (find-class class-name)
       for location = (cadr class/location-pair)
       do
         (setf (gethash class (class-hash-of store)) location)
         (%read-class-instances store class location))))

;;; ----------------------------------------------------------------------------

(defun %read-loom-store-untracked-objects (store)
  (let* ((root-node (loom-node-get (root-loc-of store)))
         (untracked-nodes (collect-linked-nodes
                           (%loom-root-untracked-objects-loc root-node)))
         (elements (mapcan #'%loom-untracked-object-pairs untracked-nodes)))
    (loop
       for pair in elements
       for type = (car pair)
       for object-loc = (cadr pair)
       for object = (read-from-location type object-loc)
       do
         (setf (gethash object (untracked->location-of store)) object-loc
               (gethash object-loc (location->untracked-of store)) object))))

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
        location)
    (loop
       with node-stack = nil
       while (plusp insert-size)
       for use = (min *max-linked-node-length* insert-size)
       for nval = (linked-node-create node (subseq objects 0 use))
       do
         (decf insert-size use)
         (setf objects (subseq objects use))
         (push nval node-stack)
       finally
         (loop for nval in node-stack
            do
              (when location
                (setf (linked-node-next nval) location))
              (multiple-value-bind (nval nloc)
                  (setf (loom-node-get) nval)
                (declare (ignore nval))
                (setf location nloc)))
         ;; Return first allocated location
         (return location))))

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
                   (loom-node-get location) node)))
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
      (setf (loom-node-get last-location) last-node))
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
                     (loom-store-sell location)
                     (setf (loom-node-get location) nil))
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
              (setf (loom-node-get location) node))))
        loc/node/mod-triples))

;;; ----------------------------------------------------------------------------

(defun remove-from-linked-node (predicate location
                                &key count)
  "Remove elements from the linked nodes at location where predicate evaluates
to t when called on the element. If count is a number, terminate after the first
count deletions. Nodes before the last node are guaranteed to have >= to
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
;;; Loom store reader/writer methods
;;; 
;;; ============================================================================

;;;
;;; Dependency methods
;;;

(defgeneric read-location-dependencies (type location)
  (:documentation "Returns a list of type/location pairs that this location 
depends on."))

;;; ----------------------------------------------------------------------------

(defmethod read-location-dependencies (type location)
  (declare (ignore type location))
  nil)

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

;;; ----------------------------------------------------------------------------

(defvar *loom-in-string* nil)

(defmethod read-from-location :around (type location)
  (declare (ignore type))
  (let ((*loom-in-string* (with-loom-store (*loom-store*)
                            (archive-touch location))))
    (call-next-method)))

;;;
;;; Writer methods
;;;

(defgeneric write-to-location (object location)
  (:argument-precedence-order location object)
  (:documentation "Writes an object to the location, with updates to
*loom-store*. Users should specialize on object, and return a string."))

;;; ----------------------------------------------------------------------------

(defvar *loom-out* nil
  "A stream accessible by writer methods.")

(defmethod write-to-location :around (object location)
  (when (eq location t)
    (setf location (random-vacant-archive-loc))
    (archive-buy location
                 (usage-loc-of *loom-store*)))
  (let ((string (catch 'write-to-location-result
                  (with-output-to-string (*loom-out*)
                    (with-standard-io-syntax
                      (let ((*package* (package-of *loom-store*)))
                        (call-next-method object location)))))))
    (when (stringp string)
      (with-loom-store (*loom-store*)
        (archive-write location string (usage-loc-of *loom-store*))
        (values location (determine-class object))))))

;;; ----------------------------------------------------------------------------

(defmethod write-to-location (object location)
  (declare (ignore location))
  (error "Cannot write: unknown type ~A [~A]" (type-of object) object))

;;;
;;; Reader/Writer pairs
;;;

(defmethod write-to-location ((object cons) location)
  (unless (tree? object)
    (error "Circular lists not supported in #'write-to-location"))
  (let (deps)
    (labels ((loc-tree (tree &aux col)
               (mapcar
                (lambda (el)
                  (if (consp el)
                      (push (cons :list (loc-tree el)) col)
                      (let ((loc (persist-thing el))
                            (type (class-name (determine-class el))))
                        (push (list type loc) deps)
                        (push (list type loc) col))))
                tree)
               (nreverse col)))
      (let ((tree (loc-tree object)))
        (setf (gethash location (untracked-dependencies-of *loom-store*))
              deps)
        (prin1 (cons :list tree) *loom-out*)))))

;;; ----------------------------------------------------------------------------

(defmethod read-from-location ((type (eql 'cons)) location)
  (let ((collect '(nil)) dependencies)
    (traverse (lambda (x)
                (let ((type (car x)) (loc (cadr x)))
                  (push (load-loom-location type loc) (car collect))
                  (push (list type loc) dependencies)))
              (loom-store-get location)
              :nodes #'cdr
              :pre (lambda (y)
                     (declare (ignore y))
                     (push nil collect))
              :post (lambda (z)
                      (declare (ignore z))
                      (push (nreverse (pop collect)) (car collect)))
              :node? (lambda (m)
                       (and (consp m) (eq (car m) :list))))
    (setf (gethash location (untracked-dependencies-of *loom-store*))
          dependencies)
    (values (nreverse (car collect))
            dependencies)))

;;; ----------------------------------------------------------------------------

(defmethod read-location-dependencies ((type (eql 'cons)) location)
  (let (dependencies)
    (traverse (lambda (x)
                (let ((type (car x)) (loc (cadr x)))
                  (push (list type loc) dependencies)))
              (loom-store-get location)
              :nodes #'cdr
              :node? (lambda (x) (and (consp x) (eq (car x) :list))))
    (setf (gethash location (untracked-dependencies-of *loom-store*))
          dependencies)
    dependencies))

;;;
;;; Standard readers/writers
;;;

(defmacro define-standard-readers (types)
  `(progn ,@(loop for type in (eval types) collect
                 `(defmethod read-from-location ((object (eql ',type)) location)
                    (declare (ignore location))
                    (with-standard-io-syntax
                      (let ((*package* (package-of *loom-store*)))
                        (read-from-string *loom-in-string*)))))))


(defmacro define-standard-writers (types)
  `(progn ,@(loop for type in (eval types) collect
                 `(defmethod write-to-location ((object ,type) location)
                    (declare (ignore location))
                    (prin1 object *loom-out*)))))

(define-standard-readers '(number symbol))
(define-standard-writers '(number symbol))

(defmethod read-from-location ((object (eql 'string)) location)
  (declare (ignore location))
  *loom-in-string*)

(defmethod write-to-location ((object string) location)
  (declare (ignore location))
  (throw 'write-to-location-result object))

;;;
;;; Equality
;;;

(defgeneric loom-equality-test (thing1 thing2)
  (:documentation "Tests whether objects are to be considered equal for
persist purposes."))

;;; ----------------------------------------------------------------------------

(defmethod loom-equality-test (a b)
  (equal a b))

;;;
;;; Types
;;;

(defun determine-class (object)
  "Returns the type that object would specialize to if it was sent to
write-to-location"
  (let* ((gf (ensure-generic-function 'write-to-location))
         (specialized-types (remove-duplicates
                             (mapcar (lambda (x)
                                       (car (method-specializers x)))
                                     (generic-function-methods gf))
                             :test #'eq))
         (types (topo-sort specialized-types
                           :key #'identity
                           :depends
                           (let ((hash (fill-keys-hash
                                        'eq specialized-types)))
                             (lambda (x)
                               (remove-if-not
                                (lambda (y) (gethash y hash))
                                (class-precedence-list x)))))))
    (loop for type in types
       do (when (typep object type)
            (return-from determine-class type)))))

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

(defun ensure-loom-class (class)
  "Returns the location of 'class's instance-list, creating it if it does not
already exist."
  (assert (typep class 'loom-persist))
  (unless (typep *loom-store* 'loom-store)
    (return-from ensure-loom-class class))
  (with-loom-store (*loom-store*)
    (let* ((ch (class-hash-of *loom-store*))
           (loc (gethash class ch)))
      (cond ((typep loc 'loom-loc) loc)
            (t 
             (let ((location
                    (nth-value 1
                               (setf (loom-store-get)
                                     (make-%loom-node :type 'instance-list)))))
               (add-to-linked-node
                `((,(class-name class) ,location))
                (classes-loc-of *loom-store*))
               (setf (gethash class ch) location
                     (loom-store-of class) *loom-store*)
               location))))))

;;; ----------------------------------------------------------------------------

(defvar *force-loom-load* nil
  "Bind to t to force #'load-loom-instance to reload from store.")

(defun load-loom-instance (class id)
  "A memoized function to return the instance associated with a class/id pair.
Calls load-loom-location for slot locations."
  (or (and (not *force-loom-load*) (gethash id (ids->instances-of class)))
      (with-loom-store (*loom-store*)
        (let ((location (gethash (list (class-name class) id)
                                 (class/id->location-of *loom-store*))))
          (when (typep location 'loom-loc)
            (let* ((instance (or (gethash id (ids->instances-of class))
                                 (allocate-instance class)))
                   (object (loom-store-get location))
                   (*without-persisting-access* t)
                   (stored-slots
                    (let ((hash (make-hash-table :test 'eq)))
                      (mapc (lambda (x)
                              (destructuring-bind (name type location) x
                                (setf (gethash name hash)
                                      (list type location))))
                            (%loom-object-slots object))
                      hash)))
              (shared-initialize
               instance
               (remove-if (lambda (slot)
                            (gethash (slot-definition-name slot) stored-slots))
                          (class-slots class)))
              (maphash
               (lambda (key val)
                 (destructuring-bind (type location) val
                   (setf (slot-value instance key)
                         (load-loom-location type location))))
               stored-slots)
              ;; Set weak links
              (setf (gethash id (ids->instances-of class)) instance
                    (gethash instance (instances->ids-of class)) id)
              instance))))))

;;; ----------------------------------------------------------------------------

(defun load-loom-location (type location)
  (let* ((store *loom-store*)
         (untracked-hash (location->untracked-of store))
         (id-hash (location->class/id-of store)))
    (let ((untracked-cache (gethash location untracked-hash))
          (class/id (gethash location id-hash)))
      (cond (untracked-cache untracked-cache)
            ((typep (find-class type) 'loom-persist)
             (assert (eq (car class/id) type))
             (load-loom-instance (find-class type)
                                 (cadr class/id)))
            (t
             (let ((thing (read-from-location type location)))
               (setf (gethash location untracked-hash) thing
                     (gethash thing (untracked->location-of store)) location)
               thing))))))

;;; ----------------------------------------------------------------------------

(defun get-instance-id (class instance)
  (or (gethash instance (instances->ids-of class))
      (prog1 (id-counter-of class)
        (incf (id-counter-of class)))))

;;; ----------------------------------------------------------------------------

(defun class-persisted-slots (class)
  (remove-if (lambda (x)
               (find (slot-definition-name x)
                     (ignored-slots-of class)
                     :test #'eq))
             (class-slots class)))

;;; ----------------------------------------------------------------------------

(defun initially-persist-loom-instance (class instance)
  (assert (and (typep instance class)
               (typep class 'loom-persist)))
  (let ((*without-persisting-access* t)
        (class-name (class-name class)))
    (let ((slots (class-persisted-slots class))
          (loom-object (make-%loom-object :class class-name)))
      (setf (%loom-object-slots loom-object)
            (loop
               for slot in slots
               for slot-value = (slot-value-using-class class instance slot)
               collect
                 (list (slot-definition-name slot)
                       (if (typep (class-of slot-value) 'loom-persist)
                           (class-name (class-of slot-value))
                           (class-name (determine-class slot-value)))
                       (persist-thing slot-value))))
      (let* ((id (get-instance-id class instance))
             (location (or (gethash (list class-name id)
                                    (location->class/id-of *loom-store*))
                           (instantiate-archive-location)))
             (class/id `(,class-name ,id)))
        (setf (loom-store-get location) loom-object)
        (add-to-linked-node `((,id ,location))
                            (ensure-loom-class class))
        (setf (gethash id (ids->instances-of class)) instance
              (gethash instance (instances->ids-of class)) id
              (gethash instance (instances-of *loom-store*)) instance)
        (setf (gethash class/id (class/id->location-of *loom-store*))
              location
              (gethash location (location->class/id-of *loom-store*))
              class/id)
        location))))

;;; ----------------------------------------------------------------------------

(defun %commit-loom-slot-changes
    (location loom-object new-slots)
  (let* ((hash (fill-keys-hash 'eq (mapcar #'car new-slots)))
         (old-slots (remove-if (lambda (x)
                                 (or (not x)
                                     (gethash (car x) hash)))
                               (%loom-object-slots loom-object))))
    (setf (%loom-object-slots loom-object)
          (nconc new-slots old-slots))
    (setf (loom-store-get location) loom-object)))

;;; ----------------------------------------------------------------------------

(defun %persist-loom-slots-of-loom-persist
    (slot value current-def)
  (let* ((class (class-of value))
         (id (gethash value (instances->ids-of class)))
         (c/id `(,(class-name class) ,id))
         (slot-location
          (gethash c/id (class/id->location-of *loom-store*)))
         (current-type (elt current-def 1))
         (current-loc (elt current-def 2)))
    (unless (and (eq (class-name class) current-type)
                 (string= current-loc slot-location))
      (list (slot-definition-name slot)
            (class-name class)
            current-loc))))

;;; ----------------------------------------------------------------------------

(defun %persist-loom-slots-of-untracked
    (slot value current-def)
  (let ((current-location (elt current-def 2))
        (type (determine-class value)))
    (unless (and 
             (eq type (elt current-def 1))
             slot-location (string= slot-location current-location))
      (list (slot-definition-name slot)
            (class-name type)
            (persist-thing value current-location)))))
  
;;; ----------------------------------------------------------------------------

(defun %persist-loom-slots (class instance loom-object slots-or-t)
  "Takes a class, instance, %loom-object, and slot names list. Persists new
untracked objects and links loom-objects."
  (let ((*without-persisting-access* t)
        (slots (if (eq slots-or-t t)
                   (class-persisted-slots class)
                   (find-slots class slots-or-t)))
        (location (gethash `(,(class-name class)
                             ,(gethash instance (instances->ids-of class)))
                           (class/id->location-of *loom-store*))))
    (%commit-loom-slot-changes
     location loom-object
     (mapcar
      (lambda (slot)
        (let ((value (slot-value-using-class
                      class instance slot))
              (def (find (slot-definition-name slot)
                         (%loom-object-slots loom-object)
                         :key #'car :test #'eq)))
          (typecase (class-of value)
            (loom-persist (%persist-loom-slots-of-loom-persist
                           slot value def))
            (otherwise (%persist-loom-slots-of-untracked
                        slot value def)))))
      slots))))

;;; ----------------------------------------------------------------------------

(defun persist-loom-slots (class instance slots-or-t)
  (let* ((id (gethash instance (instances->ids-of class)))
         (loc (gethash `(,(class-name class) ,id)
                       (class/id->location-of *loom-store*)))
         (loom-object (loom-store-get loc)))
    (%persist-loom-slots class instance loom-object slots-or-t)))

;;; ----------------------------------------------------------------------------
                                          
(defun persist-thing (thing &key (location t))
  "Persists a thing, returning the thing's location."
  (let ((class (class-of thing)))
    (typecase class
      (loom-persist
       (let* ((id (gethash thing (instances->ids-of class)))
              (location (gethash `(,(class-name class) ,id)
                                 (class/id->location-of *loom-store*))))
         (or location
             (initially-persist-loom-instance (class-of thing) thing))))
      (otherwise
       (multiple-value-bind (location type)
           (write-to-location thing location)
         (add-to-linked-node
          `((,(class-name type) ,location))
          (untracked-objects-loc-of *loom-store*))
         (let ((luh (location->untracked-of *loom-store*))
               (ulh (untracked->location-of *loom-store*)))
           (setf (gethash location luh) thing
                 (gethash thing ulh) location))
         location)))))
    
;;; ----------------------------------------------------------------------------

(defun map-loom-instances-of-class (fn class)
  (when (symbolp class)
    (setf class (find-class class)))
  (when (typep class 'loom-persist)
    (dotimes (id (id-counter-of class))
      (awhen (load-loom-instance class id)
        (funcall fn it)))))

(defmacro do-loom-instances-of-class ((instance class) &body body)
  (let ((thunk (gensym "THUNK")))
    `(block nil
       (flet ((,thunk (,instance)
                ,@body))
         (declare (dynamic-extent #',thunk))
         (map-loom-instances-of-class #',thunk ,class)))))

;;; ----------------------------------------------------------------------------

(defun remove-loom-object (thing)
  (let ((class (class-of thing)))
    (check-type class loom-persist)
    (let* ((id (gethash thing (instances->ids-of class)))
           (class/id `(,(class-name class) ,id))
           (location (gethash class/id
                              (class/id->location-of *loom-store*)))
           (object (loom-store-get location)))
      ;; Remove from loom class instance-list
      (remove-from-linked-node
       (lambda (x)
         (destructuring-bind (store-id store-location)
             x
           (when (= id store-id)
             (assert (string= location store-location))
             t)))
       (gethash class (class-hash-of *loom-store*)))
      ;; Wipe loom-store hashes of thing
      (remhash `(,(class-name class) ,id)
               (class/id->location-of *loom-store*))
      (remhash location (location->class/id-of *loom-store*))
      (remhash thing (instances-of *loom-store*))
      ;; Wipe class hashes of thing
      (remhash id (ids->instances-of class))
      (remhash thing (instances->ids-of class))
      
      (let (children)
        (mapc (lambda (ntl)
                (let ((name (car ntl))
                      (type (cadr ntl))
                      (loc (caddr ntl)))
                  (declare (ignore name))
                  (when (not (typep (find-class type) 'loom-persist))
                    (push loc children)
                    (push (get-dependencies type loc) children))))
              (%loom-object-slots object))
        (mapc #'loom-store-sell children))
      
      ;; Remove instance node
      (loom-store-sell location))))

      (maphash (lambda (untracked location)
                 (unless (gethash location mark)
                   ;; It would be faster to delete all of these
                   ;; from an in-memory copy of the nodes, then
                   ;; write when done, but this works for now.
                   (remove-from-linked-node
                    (lambda (x)
                      (destructuring-bind (type store-location)
                          x
                        (declare (ignore type))
                        (equal location store-location)))
                    (untracked-objects-loc-of *loom-store*))
                   (remhash untracked (untracked->location-of *loom-store*))
                   (remhash location (location->untracked-of *loom-store*))
                   (remhash location (untracked-dependencies-of *loom-store*))
                   (loom-store-sell location)))
               (untracked->location-of *loom-store*)))))

;;; ----------------------------------------------------------------------------

(defmethod make-instance :around ((class loom-persist) &rest args)
  (declare (ignore args))
  (let* ((*without-persisting-access* t)
         (instance (call-next-method)))
    (initially-persist-loom-instance class instance)
    instance))

;;; ----------------------------------------------------------------------------

(defmethod (setf slot-value-using-class) :after
    (value (class loom-persist) instance slot)
  (declare (ignore value))
  (unless *without-persisting-access*
    (persist-loom-slots class instance (list (slot-definition-name slot)))))
  
;;; ----------------------------------------------------------------------------

(defmethod finalize-inheritance :after ((class loom-persist))
  (let ((*defining-class* class))
    (ensure-loom-class class)))

;;; ----------------------------------------------------------------------------

(defun map-tracked-slots (fn instance)
  (let* ((class (class-of instance))
         (ignored (fill-keys-hash 'eq (ignored-slots-of class))))
    (mapc (lambda (slotd)
            (funcall fn (slot-value-using-class class instance slotd)))
          (remove-if
           (lambda (slotd &aux (slot-name (slot-definition-name slotd)))
             (gethash slot-name ignored))
           (class-slots (class-of instance))))))

;;; ----------------------------------------------------------------------------

(defun get-dependencies (type location)
  (or (gethash location (untracked-dependencies-of *loom-store*))
      (read-location-dependencies type location)))

;;; ----------------------------------------------------------------------------

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
