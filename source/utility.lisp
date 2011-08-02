(in-package :loom)

;;;; ===========================================================================
;;;; ===========================================================================
;;;;
;;;; Loom utility definitions
;;;;
;;;; ===========================================================================
;;;; ===========================================================================

(defun topo-sort (list &key key depends)
  "List is a list of nodes, key is hashed with eql in the visited table, and
depends returns a list of nodes. Does not detect cycles."
  (let ((sort '()) (vis (make-hash-table :test 'eql)))
    (labels ((visit (node &aux (key (funcall key node)))
               (unless (gethash key vis)
                 (setf (gethash key vis) t)
                 (mapc #'visit (funcall depends node))
                 (push node sort))))
      (mapc (lambda (x) (setf (gethash (funcall key x) vis) nil)) list)
      (mapc #'visit list))
    sort))

;;; ----------------------------------------------------------------------------

(defun fill-hash (test keys values)
  (let ((hash (make-hash-table :test test)))
    (loop
       for key in keys
       for value in values
       do
         (setf (gethash key hash) value))
    hash))

;;; ----------------------------------------------------------------------------

(defun fill-keys-hash (test keys)
  (let ((hash (make-hash-table :test test)))
    (loop
       for key in keys
       do
         (setf (gethash key hash) t))
    hash))

;;; ----------------------------------------------------------------------------

(defun find-slots (class slot-names)
  (let ((hash (fill-keys-hash 'eq slot-names)))
    (remove-if-not (lambda (slot)
                     (gethash (slot-definition-name slot) hash))
                   (class-slots class))))

;;; ----------------------------------------------------------------------------

(defun mlambda (gf args body)
  (make-method-lambda
   gf (allocate-instance
       (generic-function-method-class gf))
   `(lambda ,args ,@body)
   nil))

;;; ----------------------------------------------------------------------------

(defvar *defining-class* nil)

(defun defining-find-class (symbol)
  (if (and *defining-class*
           (eq (class-name *defining-class*) symbol))
      *defining-class* (find-class symbol)))

;;; ----------------------------------------------------------------------------

(defun add-method-to-gf
    (gf-name qualifiers spec decl lambda-body)
  (let* ((gf (if (typep gf-name 'generic-function)
                 gf-name (ensure-generic-function gf-name)))
         (lambda-list (extract-lambda-list spec))
         (code (mlambda gf lambda-list lambda-body)))
    (setf code
          `(lambda ,(cadr code) (declare ,@decl) ,@(cddr code)))
    (add-method
     gf (make-instance
         (generic-function-method-class gf)
         :specializers
         (mapcar (lambda (x)
                   (cond ((and (consp x) (eq (car x) 'eql))
                          (intern-eql-specializer (cadr x)))
                         ((symbolp x) (defining-find-class x))
                          ((classp x) x)
                          (t (error "Invalid specializer ~a" x))))
                 (extract-specializer-names spec))
         :lambda-list lambda-list
         :qualifiers qualifiers
         :function (compile nil code)))))

;;; ----------------------------------------------------------------------------

(defun compare-specializer-lists (a b)
  (loop for at in a for bt in b
     do (etypecase at
          (eql-specializer
           (unless (and (typep bt 'eql-specializer)
                        (eql (eql-specializer-object at)
                             (eql-specializer-object bt)))
             (return-from compare-specializer-lists nil)))
          (class
           (unless (and (typep bt 'class)
                        (eq at bt))
             (return-from compare-specializer-lists nil)))))
  t)

;;; ----------------------------------------------------------------------------

(defun find-method-of-gf (gf &key plural qualifiers specializers)
  (let* ((gf (if (typep gf 'generic-function)
                 gf (ensure-generic-function gf)))
         (methods (generic-function-methods gf))
         (spec (when specializers
                 (mapcar
                  (lambda (x)
                    (cond ((and (consp x)
                                (eq (car x) 'eql))
                           (intern-eql-specializer
                            (cadr x)))
                          (t (defining-find-class x))))
                  specializers)))
         (filter (remove-if-not
                  (lambda (method)
                    (block remove
                      (when qualifiers
                        (unless (equal (method-qualifiers method) qualifiers)
                          (return-from remove nil)))
                      (when specializers
                        (unless (compare-specializer-lists
                                 (method-specializers method) spec)
                          (return-from remove nil)))
                      t))
                  methods)))
    (if plural
        filter
        (when (= (length filter) 1)
          (car filter)))))

;;; ----------------------------------------------------------------------------

(defun remove-method-from-gf (gf &key plural qualifiers specializers)
  (let* ((gf (if (typep gf 'generic-function)
                 gf (ensure-generic-function gf)))
         (method (find-method-of-gf gf
                                    :plural plural
                                    :qualifiers qualifiers
                                    :specializers specializers)))
    (if (consp method)
        (mapc (lambda (x)
                (remove-method gf x))
              method)
        (remove-method gf method))))

;;; ----------------------------------------------------------------------------

(defun tree? (tree)
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((test-mark (cons)
               (prog1 (gethash cons seen)
                 (setf (gethash cons seen) t)))
             (descend-tree (cons)
               (mapl (lambda (el)
                       (when (consp el)
                         (when (test-mark el)
                           (return-from tree? nil))
                         (when (consp (car el))
                           (descend-tree (car el)))))
                     cons)))
      (when (consp tree)
        (descend-tree tree)
        t))))

;;; ----------------------------------------------------------------------------

(defun traverse (leaf tree &key (nodes #'identity) pre post (node? #'consp))
  (unless (consp tree) (return-from traverse nil))
  (mapc (lambda (el)
          (if (funcall node? el)
              (progn
                (when pre (funcall pre el))
                (traverse leaf el :nodes nodes :pre pre :post post :node? node?)
                (when post (funcall post el)))
              (funcall leaf el)))
        (funcall nodes tree)))

;;; ----------------------------------------------------------------------------

(defun parse-hex (string)
  (declare 
   (optimize (speed 3) (safety 0) (compilation-speed 0) (space 0) (debug 0))
   (type string string))
  (flet ((digit (x)
           (let ((code (char-code x)))
             (declare (type fixnum code))
             (cond ((and (<= code 57) (>= code 48))
                    (the fixnum (- code 48)))
                   ((and (>= code 97) (<= code 102))
                    (the fixnum (- code 87)))
                   ((and (<= code 70) (>= code 65))
                    (the fixnum (- code 55)))
                   (t (error "not a hexadecimal digit"))))))
    (declare (inline digit))
    (let ((num 0) (len (length string)))
      (declare (type integer num)
               (type fixnum len))
      (do ((x 0 (incf x)))
          ((= x len) num)
        (declare (type fixnum x))
        (setf num (+ (digit (aref string x)) (ash num 4)))))))

;;; ----------------------------------------------------------------------------

(defun print-hex (integer &key (padding 0))
  (declare 
   (optimize (speed 3) (safety 0) (compilation-speed 0) (space 0) (debug 0))
   (type integer integer padding))
  (let* ((chars "0123456789abcdef")
         (len (max padding (ceiling (log integer 16))))
         (ret (make-array len :element-type 'character)))
    (declare
     (type fixnum len)
     (type (vector character) chars ret))
    (do ((x (* (1- len) 4) (1- x)) (el 0 (1+ el)))
        ((= el len) ret)
      (declare (type fixnum el x))
      (setf (aref ret el)
            (aref chars (ldb (byte 4 x) integer))))))

;;; ============================================================================
;;; EOF
;;; ============================================================================