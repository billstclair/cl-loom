;;;; -*- mode: lisp -*-

(in-package :loom)

;;;;
;;;; C-style escaping of strings
;;;;

(in-package :loom)

(defparameter *c-escape-code-alist*
  '((#\a . #o007)                          ; Bell (alert)
    (#\b . #o177)                          ; Backspace
    (#\f . #o014)                          ; Formfeed
    (#\n . #o012)                          ; New line
    (#\r . #o015)                          ; Carriage return
    (#\t . #o011)                          ; Horizontal tab
    (#\v . #o013)                          ; Vertical tab
    ))

(defun c-escape (string)
  (check-type string string)
  (with-output-to-string (s)
    (dotimes (i (length string))
      (let* ((c (elt string i))
             (code (char-code c)))
        (if (<= 20 code 126)
            (write-char c s)
            (let ((cell (rassoc code *c-escape-code-alist*)))
              (if cell
                  (format s "\\~a" (car cell))
                  (let ((code (char-code c)))
                    (when (> code 255) (error "Can only encode 8-bit chars"))
                    (format s "\\~3,'0o" (char-code c))))))))))

(defun c-unescape (string)
  (check-type string string)
  (with-input-from-string (s string)
    (with-output-to-string (os)
      (loop for char = (read-char s nil :eof)
         do
           (when (eq char :eof) (return))
           (cond ((not (eql char #\\)) (write-char char os))
                 (t (setf char (read-char s))
                    (cond ((<= #.(char-code #\0) (char-code char) #.(char-code #\9))
                           (let ((code (parse-integer (format nil "~a~a~a"
                                                              char
                                                              (read-char s)
                                                              (read-char s))
                                                      :radix 8)))
                             (write-char (code-char code) os)))
                          (t (let ((cell (assoc char *c-escape-code-alist*)))
                               (if cell
                                   (write-char (code-char (cdr cell)) os)
                                   (write-char char os)))))))))))
