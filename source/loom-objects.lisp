;;;; -*- mode: lisp -*-

(in-package :loom)

;;;;
;;;; Loom objects
;;;; A simple persistence mechanism for storing
;;;; lisp objects in the loom archive
;;;;

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

(defmethod print-object ((class loom-class) stream)
  (print-unreadable-object (class stream :type t)
    (prin1 (class-name-of class))))

(defun make-loom-class (class-name slots &optional instances-loc)
  (make-instance 'loom-class
                 :class-name class-name
                 :slots slots
                 :instances-loc instances-loc))

(defparameter *loom-class-slots*
  '(class-name slots instances-loc))

(defclass loom-store ()
  ((server :initarg :server :accessor server-of :type loom-server)
   (usage-loc :initarg :usage-loc
              :initform nil
              :accessor usage-loc-of
              :type (or loom-loc null))
   (root-loc :initarg :root-loc
             :initform nil
             :accessor root-loc-of
             :type (or loom-loc null))
   (classes-loc :initarg :classes-loc
                :initform nil
                :accessor classes-loc-of
                :type (or loom-loc null))
   (package :initarg :package
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
   (class-hash :initform (make-hash-table :test 'eq)
               :accessor class-hash-of
               :documentation "class-name => loom-class instance")
   ))

(defmethod initialize-instance :after ((store loom-store)
                                       &key &allow-other-keys)
  (initialize-loom-store store))

(defmethod print-object ((store loom-store) stream)
  (print-unreadable-object (store stream :type t)
    (format stream "~s ~s"
            (base-uri-of (server-of store))
            (root-loc-of store))))

(defun make-loom-store (&key (server *loom-server*)
                        root-loc
                        usage-loc
                        package)
  "Make a database to store lisp objects in a loom archive.
SERVER is a LOOM-SERVER, default: *LOOM-SERVER*.
ROOT-LOC, if specified, is a previous MAKE-LOOM-STORE result.
  Will open that database for access.
  If ROOT-LOC is NIL, the default, will create a new database.
USAGE-LOC is a location from which to take usage tokens for the write.
  Must be specified and contain usage tokens if ROOT-LOC is NIL.
  Defaults to the USAGE-LOC used to create the database otherwise.
PACKAGE is the package to use for printing strings for serialization
  to the database. Defaults to *PACKAGE*. Ignored if ROOT-LOC is specified.
Returns a LOOM-STORE instance."
  (check-type server loom-server)
  (check-type usage-loc (or loom-loc null))
  (check-type root-loc (or loom-loc null))  ;null to initialize a new store
  (if (null package)
      (setf package *package*)
      (unless (typep package 'package)
        (setf package
              (or (find-package package)
                  (error "There is no package named: ~s" package)))))
  (make-instance 'loom-store
                 :server server
                 :usage-loc usage-loc
                 :root-loc root-loc
                 :package package))

(defvar *loom-store* nil)

(defmacro with-loom-store ((loom-store) &body body)
  "Binds *LOOM-STORE* to LOOM-STORE and executes BODY with a transaction
on LOOM-STORE's server."
  `(let ((*loom-store* ,loom-store))
     (with-loom-transaction (:server (server-of *loom-store*))
       ,@body)))

(defun initialize-loom-store (store)
  (with-loom-store (store)
    (let ((root-loc (root-loc-of store)))
      (if root-loc
          (%read-loom-store-classes store root-loc)
          (%initialize-new-loom-store store)))))

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

(defvar *loom-read-location* nil)

(defun loom-store-get (loc &optional usage-loc)
  (declare (ignore usage-loc))
  (with-loom-store-reader ()
    (let ((*loom-read-location* loc))
      (read-from-string (archive-touch loc)))))

(defun (setf loom-store-get) (value loc &optional usage-loc)
  (with-standard-io-syntax
    (let ((*package* (or *loom-reader-package* (package-of *loom-store*)))
          (*print-circle* t)
          (*print-readably* t))
      (archive-write loc
                     (with-output-to-string (s)
                       (write-to-loom-store value s))
                     (or usage-loc (usage-loc-of *loom-store*)))
      value)))

(defun %initialize-new-loom-store (store)
  (let* ((root-loc (random-vacant-archive-loc))
         (classes-loc (random-vacant-archive-loc))
         (usage-loc (usage-loc-of store))
         (package (package-of store)))
    (check-type usage-loc loom-loc)
    (check-type package package)
    (archive-buy root-loc usage-loc)
    (archive-buy classes-loc usage-loc)
    (let ((root-plist (list :type 'loom-store
                            :classes-loc classes-loc
                            :package (package-name package)
                            :usage-loc usage-loc))
          (classes `((loom-class nil . nil))))
      (setf (loom-store-get classes-loc) classes)
      (let ((*loom-reader-package* *loom-package*))
        (setf (loom-store-get root-loc) root-plist))
      (setf (root-loc-of store) root-loc
            (classes-loc-of store) classes-loc
            (gethash 'loom-class (class-hash-of store))
            (make-loom-class 'loom-class *loom-class-slots* classes-loc))))
  store)

(defun loom-class-node-p (list)
  (loom-linked-node-p list 'loom-class))

(defun loom-linked-node-p (list class-name)
  (and (listp list)
       (let ((links (car list)))
         (and (listp links)
              (eq (car links) class-name)
              (listp (cdr links))
              (typep (cadr links) '(or null loom-loc))
              (typep (cddr links) '(or null loom-loc))))))

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

(defun %read-loom-store-classes (store root-loc)
  (let ((root-plist (let ((*loom-reader-package* *loom-package*))
                      (loom-store-get root-loc))))
    (assert (and (listp root-plist)
                 (eq (getf root-plist :type) 'loom-store)))
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

;;;
;;; Reader dispatching
;;;

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
