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
   (instances
    :accessor instances-of
    :initform (make-weak-hash-table :weakness :value
                                    :test #'eql)
    :type hash-table
    :documentation "A weak hashtable mapping instance id's -> instances"))
  (:documentation
   "A metaclass, classes of which will have themselves, and instances of
themselves be persisted to the current *loom-store*."))

(defclass loom-class ()
  ((class-name :initarg :class-name
               :accessor class-name-of
               :documentation "The name of the user class")
   (slots :initarg :slots :accessor slots-of :type list
          :documentation "List of slot names")
   (instances-loc :initarg :instances-loc
                  :initform nil
                  :accessor instances-loc-of
                  :type (or loom-loc null)
                  :documentation "Location of linked list of instance locations")))

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
   (loc-to-instance-hash
    :initform (make-hash-table :test 'equal)
    :accessor loc-to-instance-hash-of
    :documentation "location->  instance")
   (instance-to-loc-hash
    :initform (make-hash-table :test 'equal)
    :accessor instance-to-loc-hash-of
    :documentation "instance->  location")
   (class-hash
    :initform (make-hash-table :test 'eq)
    :accessor class-hash-of
    :documentation "class-name => loom-class instance")
   ))

;;; 
;;; loom-class methods
;;; 

(defmethod print-object ((class loom-class) stream)
  (print-unreadable-object (class stream :type t)
    (prin1 (class-name-of class))))

;;; ----------------------------------------------------------------------------

(defun make-loom-class (class-name slots &optional instances-loc)
  (make-instance 'loom-class
                 :class-name class-name
                 :slots slots
                 :instances-loc instances-loc))

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

(defstruct %loom-object
  "Defines the structure used to serialize object instances within the store.
'Class' stores the symbol-name of this object's class, and 'slots' stores
a list of slot-name/slot-type/slot-location triples (symbol symbol string).
The function hashed at slot-type from *loom-value-[readers/writers]* will be
used to read the leaf and return a lisp object."
  (class nil :type (or symbol null))
  (slots nil :type (or cons null)))

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
  (unless (and loc (typep loc 'loom-loc))
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

;;; ============================================================================
;;; 
;;; Loom store creation and initialization
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
               (setf (%loom-store-get nil package-name usage-loc)
                     (make-%loom-node :type 'class-list))
               :untracked-objects-loc
               (setf (%loom-store-get nil package-name usage-loc)
                     (make-%loom-object))
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
                     :class-loc (%loom-root-class-loc root-node)
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

(defun %read-loom-store-classes (store)
  (let* ((root-node (loom-store-get (root-loc-of store)))
         (class-node (loom-store-get (%loom-root-class-loc root-node))))
    (assert (eq (%loom-node-type class-node) 'class-node))
            
    #+no(assert (and (listp root-plist)
                 (eq (getf root-plist :type) 'loom-store)))
    #+no
    (let* ((classes-loc (getf root-plist :classes-loc))
           (package-name (getf root-plist :package))
           (package (or (find-package package-name)
                        (error "There is no package named ~s" package-name)))
           (usage-loc (getf root-plist :usage-loc))
           (class-hash (class-hash-of store))
           (loom-class-class (find-class 'loom-class)))
      (setf (classes-loc-of store) classes-loc
            (package-of store) package)
      (unless (usage-loc-of store)
        (setf (usage-loc-of store) usage-loc))
      (setf (gethash 'loom-class class-hash)
            (make-loom-class 'loom-class *loom-class-slots* classes-loc))
      (do-linked-nodes (loc classes-loc 'loom-class)
        (let ((class (instantiating-class (*instantiating-instance*
                                           loom-class-class loc)
                       (loom-store-get loc))))
          ;; This will error if the class isn't defined, as desired.
          (find-class (class-name-of class))
          (setf (gethash (class-name-of class) class-hash) class))))))

;;; ----------------------------------------------------------------------------

(defun %read-loom-store-untracked-objects (store)
  ;
  )

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :after ((store loom-store)
                                       &rest args)
  (declare (ignore args))
  (with-loom-store (store)
    (%read-loom-store-classes store)
    (%read-loom-store-untracked-objects store)))

;;; ----------------------------------------------------------------------------

(defun loom-class-node-p (list)
  (loom-linked-node-p list 'loom-class))

;;; ----------------------------------------------------------------------------

(defun loom-linked-node-p (list class-name)
  (and (listp list)
       (let ((links (car list)))
         (and (listp links)
              (eq (car links) class-name)
              (listp (cdr links))
              (typep (cadr links) '(or null loom-loc))
              (typep (cddr links) '(or null loom-loc))))))

;;; ----------------------------------------------------------------------------
;;; Read table stuff -- deprecated
;;; ----------------------------------------------------------------------------

(defparameter *loom-store-readtable*
  (let ((rt (copy-readtable)))
    (set-dispatch-macro-character #\# #\G 'loom-store-g-dispatch rt)
    rt))

(defun loom-store-g-dispatch (stream char n)
  (declare (ignore char n))
  (let ((char (read-char stream)))
    (loom-store-reader-dispatch stream char)))

(defmacro with-loom-store-reader (() &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk () ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-loom-store-reader #',thunk))))

(defparameter *loom-package* (find-package :loom))

;; Override (package-of loom-store) during initialization
(defvar *loom-reader-package* nil)

(defun call-with-loom-store-reader (thunk)
  (with-standard-io-syntax
    (let ((*package* (or *loom-reader-package*
                         (package-of *loom-store*)))
          (*read-eval* nil)
          (*readtable* *loom-store-readtable*))
      (funcall thunk))))



(defun map-linked-nodes (function node-loc class-name)
  (loop for nl = node-loc then (cddr links)
     for node = (if nl
                    (loom-store-get nl)
                    (return))
     for links = (progn (assert (loom-linked-node-p node class-name))
                        (car node))
     do
       (mapc function (cdr node))))

(defmacro do-linked-nodes ((loc-var node-loc class-name) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,loc-var) ,@body))
       (declare (dynamic-extent #',thunk))
       (map-linked-nodes #',thunk ,node-loc ,class-name))))

(defparameter *max-linked-node-length* 100)

(defun add-to-linked-node (store node-loc)
  (let ((node (loom-store-get node-loc))
        (usage-loc (usage-loc-of store)))
    (when (> (length node) *max-linked-node-length*)
      (setf (cadar node) node-loc)
      (let ((new-loc (random-vacant-archive-loc)))
        (archive-buy new-loc usage-loc)
        (archive-write new-loc node usage-loc)
        (setf node `((,(caar node) nil . ,new-loc)))))
    (let ((new-loc (random-vacant-archive-loc)))
      (archive-buy new-loc usage-loc)
      (push new-loc (cdr node))
      (setf (loom-store-get node-loc usage-loc) node)
      new-loc)))

(defmacro instantiating-class ((instance-var class loc) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,instance-var) ,@body))
       (declare (dynamic-extent #',thunk))
       (call-instantiating-class #',thunk ,class ,loc))))

(defun call-instantiating-class (thunk class loc)
  (unless (typep class 'class)
    (setf class (find-class class)))
  (let ((store *loom-store*)
        (instance (allocate-instance class))
        (done nil))
    (setf (gethash instance (instance-to-loc-hash-of store)) loc
          (gethash loc (loc-to-instance-hash-of store)) instance)
    (unwind-protect
         (multiple-value-prog1
             (funcall thunk instance)
           (setf done t))
      (unless done
        (remhash instance (instance-to-loc-hash-of store))
        (remhash loc (loc-to-instance-hash-of store))))))

(defvar *instantiating-instance* nil)


;;;
;;; Reader dispatching
;;;

(defmethod loom-store-reader-dispatch (stream (char (eql #\R)))
  (check-type *loom-store* loom-store)
  (destructuring-bind (class loc) (read stream)
    (let ((res (or (gethash loc (loc-to-instance-hash-of *loom-store*))
                   (instantiating-class (*instantiating-instance* class loc)
                     (read-from-string (archive-touch loc))))))
      (assert (typep res class))
      res)))

(defparameter *unbound-marker* '--unbound--)

(defmethod loom-store-reader-dispatch (stream (char (eql #\I)))
  (destructuring-bind (class-name . slot-values) (read stream)
    (let* ((store *loom-store*)
           (class-hash (class-hash-of store))
           (loom-class (or (gethash class-name class-hash)
                           (error "There is no loom class named ~s"
                                  class-name)))
           (slots (slots-of loom-class))
           (instance *instantiating-instance*))
      (assert (eq (type-of instance) class-name))
      (loop for slot in slots
         for value in slot-values
         when (and slot (not (eq value *unbound-marker*)))
         ;; should catch errors here and remove offending slots
         do (setf (slot-value instance slot) value))
      (shared-initialize instance t)
      instance)))
    
;;;
;;; Writer methods
;;;

(defmethod write-to-loom-store (object stream)
  (prin1 object stream))

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

(defun %loom-persist-class (store class)
  (unless (gethash (class-name class) (class-hash-of store))
    (let 

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

(defun loom-instance-persisted-slots (instance)
  (let* ((class (class-of instance))
         (omitted-slots (loom-instance-omitted-slots instance)))
    (loop for slot in  (closer-mop:class-slots class)
       for slot-name = (closer-mop:slot-definition-name slot)
       when (and (eq (closer-mop:slot-definition-allocation slot) :instance)
                 (not (member slot-name omitted-slots :test #'eq)))
       collect slot-name)))

;; Specialize this to omit slots form your classes
(defgeneric loom-instance-omitted-slots (instance)
  (:method ((instance t))
    nil)
  (:documentation "Returns a list of the names of slots of instance
to omit from the persistent store.
Specializations should usually cons onto the result of (CALL-NEXT-METHOD)."))

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

(defun loom-instances-of-class (class-or-name &optional
                                (loom-store *loom-store*))
  "Returns a list of all instances of CLASS-OR-NAME in LOOM-STORE."
  (let ((res nil))
    (do-loom-instances-of-class (instance class-or-name loom-store)
      (push instance res))
    (nreverse res)))

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

;;; ============================================================================
;;; Persistant Object Definition
;;; ============================================================================


(defmethod validate-superclass ((a loom-persist) (b standard-class))
  t)

(defmethod finalize-inheritance :after ((class loom-persist))
  (let ((

;(defmethod ensure-class-using-class (


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
