(in-package :loom)

;;;; ===========================================================================
;;;; ===========================================================================
;;;;
;;;; Loom utility definitions
;;;;
;;;; ===========================================================================
;;;; ===========================================================================

(defmacro predefine-fn (name)
  `(progn (setf (fdefinition (quote ,name))
                (lambda (&rest x)
                  (declare (ignore x)) nil))
          ',name))

;;; ----------------------------------------------------------------------------

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
