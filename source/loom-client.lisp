;;;; -*- mode: lisp -*-

(in-package :loom)

;;;; ===========================================================================
;;;; ===========================================================================
;;;;
;;;; Loom client
;;;;
;;;; ===========================================================================
;;;; ===========================================================================

;;; ----------------------------------------------------------------------------
;;; Global variables
;;; ----------------------------------------------------------------------------

(defparameter *configuration-file*
  (asdf:system-relative-pathname "cl-loom" "configuration.sexp"))

(defparameter *configuration*
  (let ((path *configuration-file*))
    (with-standard-io-syntax
      (let ((*package* (find-package :loom)))
        (with-open-file (str path :direction :input :if-does-not-exist nil)
          (and str
               (or (read str nil nil)
                   (error (format nil "Failed to read ~s" path)))))))))

(defparameter *default-configuration*
  '(loom-configuration
    (hostname "secure.loom.cc")
    (port 443)
    (path "/")
    (use-ssl t)
    (local nil)
    (base-dir :unused)
    (config-dir :unused)
    (binary-path :unused))
  "Typically configuration is loaded from <basedir>/configuration.sexp
This is used only when :default-setup-p t is passed to (make-instance 'server ...)
or when no configuration.sexp file is found.")

(defparameter *rayservers-ca-certificate-filename*
  "ca-rayservers-com.pem")

(defparameter *rayservers-ca-certificate-path*
  (merge-pathnames *rayservers-ca-certificate-filename*
                   (asdf:system-source-directory :cl-loom)))

(defun rayservers-ca-certificate-file-exists-p ()
  (probe-file *rayservers-ca-certificate-path*))

(defvar *rayservers-ca-certificate-installed-p* nil)

(defparameter *rayservers-ca-asn1*
    "-----BEGIN CERTIFICATE-----
MIIElTCCA32gAwIBAgIJALoXNnj+yvJCMA0GCSqGSIb3DQEBBQUAMIGNMQswCQYD
VQQGEwJQQTELMAkGA1UECBMCTkExFDASBgNVBAcTC1BhbmFtYSBDaXR5MRgwFgYD
VQQKEw9SYXlzZXJ2ZXJzIEdtYkgxGjAYBgNVBAMTEWNhLnJheXNlcnZlcnMuY29t
MSUwIwYJKoZIhvcNAQkBFhZzdXBwb3J0QHJheXNlcnZlcnMuY29tMB4XDTA5MTAx
OTE3MzgyMFoXDTE5MTAxNzE3MzgyMFowgY0xCzAJBgNVBAYTAlBBMQswCQYDVQQI
EwJOQTEUMBIGA1UEBxMLUGFuYW1hIENpdHkxGDAWBgNVBAoTD1JheXNlcnZlcnMg
R21iSDEaMBgGA1UEAxMRY2EucmF5c2VydmVycy5jb20xJTAjBgkqhkiG9w0BCQEW
FnN1cHBvcnRAcmF5c2VydmVycy5jb20wggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAw
ggEKAoIBAQC9rNsCCM+TNp6xDk2yxhXQOStmPTd0txFyduNAj02/nLZV4eq0ZS5n
xXBE6l3MYIMBMV3BgKiy7LsdiRJeZ5HdsV/HRZzXCQI+k4acBjlRC1ZdWMNsIR+H
QUVx2y0wgp+QpcMrgBQZdPI7PobnXCZ6+Fmc50kM7xbIsoWZUzQDpRtUymgOhnnT
4TSb1/XufFHHhDMReRA7s3Co911hzcnZJqL9gFWULlB/RI2ZeVbkp0K4lUXyMZ/R
fnOtCdAA+TkQcpzoyBETV9p5MO8KBOPBskvyGYqVcIZNuxwfC2uoKx0s5b6eMRKR
54B4mB/hIi7i0uGjzuAZdt5iDXQHYaM3AgMBAAGjgfUwgfIwHQYDVR0OBBYEFOyu
Fp80LSc1gwnq5rghs/P8bMgrMIHCBgNVHSMEgbowgbeAFOyuFp80LSc1gwnq5rgh
s/P8bMgroYGTpIGQMIGNMQswCQYDVQQGEwJQQTELMAkGA1UECBMCTkExFDASBgNV
BAcTC1BhbmFtYSBDaXR5MRgwFgYDVQQKEw9SYXlzZXJ2ZXJzIEdtYkgxGjAYBgNV
BAMTEWNhLnJheXNlcnZlcnMuY29tMSUwIwYJKoZIhvcNAQkBFhZzdXBwb3J0QHJh
eXNlcnZlcnMuY29tggkAuhc2eP7K8kIwDAYDVR0TBAUwAwEB/zANBgkqhkiG9w0B
AQUFAAOCAQEAqScS+A2Hajjb+jTKQ19LVPzTpRYo1Jz0SPtzGO91n0efYeRJD5hV
tU+57zGSlUDszARvB+sxzLdJTItK+wEpDM8pLtwUT/VPrRKOoOUBkKBshcTD4HmI
k8uJlNed0QQLP41hFjr+mYd7WM+N5LtFMQAUBMUN6dzEqQIx69EnIoVp0KB8kDwW
/QK5ogKY0g8DmRTFiV036bHQH93kLzyV6FNAldO8vBDqcTeru/uU2Kcn6a8YOfO1
T6MVYory7prWbBaGPKsGw0VgrV9OGbxhbw9EOEYSOgdejvbi9VhgMvEpDYFN7Hnq
0wiHJq5jKECf3bwRe9uVzVMrIeCap/r2uA==
-----END CERTIFICATE-----")

(defvar *ssl-certificate-temp-dir* nil)

(defun ssl-certificate-temp-dir ()
  *ssl-certificate-temp-dir*)

(defun (setf ssl-certificate-temp-dir) (dir)
  (assert (and (cl-fad:directory-pathname-p dir)
               (probe-file dir)))
  (setf *ssl-certificate-temp-dir* dir))

(defun ssl-verify-init-with-rayservers-ca-certificate (&optional force-p)
  (when (or force-p (not *rayservers-ca-certificate-installed-p*))
    (when (cond ((rayservers-ca-certificate-file-exists-p)
                 (cl+ssl::ssl-verify-init
                  :verify-locations (list *rayservers-ca-certificate-path*))
                 t)
                ((ssl-certificate-temp-dir)
                 (let ((path (merge-pathnames *rayservers-ca-certificate-filename*
                                              (ssl-certificate-temp-dir))))
                   (with-open-file (s path
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                     (write-string *rayservers-ca-asn1* s))
                   (cl+ssl::ssl-verify-init
                    :verify-locations (list path))
                   t)))
      (setf *rayservers-ca-certificate-installed-p* t))))

(defun loom-server-base-dir ()
  "Return a path to the local loom server base directory"
  (asdf:system-relative-pathname "cl-loom" "Loom/"))

(defun make-configuration (&key
                           (hostname "127.0.0.1")
                           (port 8286)
                           (path "/")
                           (use-ssl nil)
                           (local t)
                           (base-dir (loom-server-base-dir))
                           (config-dir #p"data/conf/")
                           (binary-path #p"code/loom"))
  "Make a configuration alist suitable for passing as the CONFIG arg
to MAKE-LOOM-SERVER. Defaults are for a local server."
  `(loom-configuration
    (hostname ,hostname)
    (port ,port)
    (path ,path)
    (use-ssl ,use-ssl)
    (local ,local)
    (base-dir ,base-dir)
    (config-dir ,config-dir)
    (binary-path ,binary-path)))

(defun make-uri-configuration (uri-string)
  (let* ((uri (puri:parse-uri uri-string))
         (https-p (eq :https (puri:uri-scheme uri))))
    (make-configuration
     :hostname (puri:uri-host uri)
     :port (or (puri:uri-port uri)
               (if https-p 443 80))
     :path (or (puri:uri-path uri) "/")
     :use-ssl https-p
     :local nil
     :base-dir nil
     :config-dir nil
     :binary-path nil)))

;;; ----------------------------------------------------------------------------
;;; Configuration functions
;;; ----------------------------------------------------------------------------

(defun config-option (option &optional (config *configuration*))
  (cadr (assoc option (cdr config) :test #'eq)))

;;; ----------------------------------------------------------------------------

(defun (setf config-option) (new-value option &optional (config *configuration*))
  (let ((search (assoc option (cdr config) :test #'eq)))
    (if search
        (car (rplaca (cdr search) new-value))
        (nconc config
               (list (list option new-value))))))

;;; ----------------------------------------------------------------------------

(defun config-path (name &optional (config *configuration*))
  (merge-pathnames (config-option name config)
                   (config-option 'base-dir config)))

;;; ----------------------------------------------------------------------------

(defun save-configuration (&key
                           (path *configuration-file*)
                           (config *configuration*))
  (with-open-file (file path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (let ((*print-pretty* t)
          (*print-case* :downcase))
      (pprint-linear file config t))))

;;; ----------------------------------------------------------------------------

;; The server for all the functions.
;; Bind it with-loom-server or with-loom-transaction
(defvar *loom-server* nil)

#+ccl
(ccl::define-standard-initial-binding '*loom-server*
    (lambda () nil))

(defmacro with-loom-server ((server) &body body)
  `(let ((*loom-server* ,server))
     (check-type *loom-server* loom-server)
     ,@body))

(defmacro with-server-bound ((&optional (server '*loom-server*)) &body body)
  `(let ((server ,server))
     (check-type server loom-server)
     ,@body))

;;; ----------------------------------------------------------------------------

(defun generate-uri (&optional (config *configuration*))
  (format nil "~a://~a:~a~a"
          (if (config-option 'use-ssl config) "https" "http")
          (config-option 'hostname config)
          (config-option 'port config)
          (config-option 'path config)))

;; Default setup connects to loom.cc
;; (make-instance 'loom-server :default-setup-p t) makes a local server

(defclass loom-server ()
  ((base-uri :initform nil ;; set during (initialize-instance :after))
             :initarg :base-uri
             :accessor base-uri-of)
   (config :initform *configuration*
           :initarg :config
           :accessor config-of))
  (:documentation "The URI and configuration information for a Loom server
By default, it points at loom.cc.
Pass true for the :DEFAULT-SETUP-P initarg, and it will use the default settings
for a local server."))

;;; ----------------------------------------------------------------------------

(defmethod initialize-instance :after ((server loom-server)
                                       &key default-setup-p config
                                       &allow-other-keys)
  (when (or (and default-setup-p (not config))
            (not *configuration*))
    (setf (config-of server)
          *default-configuration*))
  (setf (base-uri-of server)
        (generate-uri (config-of server)))
  (when (config-option 'local config)
    (let ((*loom-server* server))
      (attempt-loom-server-startup t))))

(defmethod print-object ((server loom-server) stream)
  (print-unreadable-object (server stream :type t)
    (format stream "~s" (base-uri-of server))))

(defun make-loom-server (&optional config)
  "Builds a loom server out of the configuration loaded from *configuration-file*,
or uses a specified configuration. Setting config to t is a shortcut to create
the loom.cc server."
  (cond
    ((and (consp config)
          (eq (car config) 'loom-configuration))
     (make-instance 'loom-server :config config))
    ((eq config t)
     (make-instance 'loom-server :default-setup-p t))
    (t (make-instance 'loom-server))))

(defun make-loom-uri-server (uri-string)
  (make-loom-server (make-uri-configuration uri-string)))

;; KV format examples: https://secure.loom.cc/?function=archive_tutorial&help=1
;; (
;; :name
;; =value
;; ...
;; )
(defun parse-kv (kv)
  "Parse a Loom key/value string. Return an alist."
  (check-type kv string)
  (let* ((lines (split-sequence:split-sequence
                 #\newline kv :remove-empty-subseqs t))
         (even t)
         (res (alexandria:plist-alist
               (mapcar (lambda (arg)
                         (cond (even (assert (eql #\: (elt arg 0))))
                               (t (assert (eql #\= (elt arg 0)))))
                         (setf even (not even))
                         (c-unescape (subseq arg 1 (length arg))))
                       (butlast (cdr lines))))))
    (assert (and (equal "(" (car lines))
                 (equal ")" (car (last lines))))
            nil
            "Malformed kv string: ~s" kv)
    res))

(defun downcase-princ-to-string (x)
  (if (stringp x)
      x
      (string-downcase (princ-to-string x))))

(defun kv-lookup (key alist)
  "Look up a key in an alist returned by parse-kv"
  (cdr (assoc (downcase-princ-to-string key) alist :test #'equal)))

(defvar *skip-response-error-check* nil)

(defun parse-response (response)
  (let* ((res (parse-kv response)))
    (unless *skip-response-error-check*
      (maybe-signal-loom-error res))
    res))

(define-condition loom-client-error (simple-error)
  ;; Errors provide most (but not all) key/value information to clients
  ((error-args :initarg :error-args :initform nil :reader loom-client-error-args)))

(defgeneric get-loom-error-property (e keyname)
  (:documentation ""))

(defmethod get-loom-error-property ((e loom-client-error) keyname)
  (cdr (find (downcase-princ-to-string keyname) (loom-client-error-args e)
             :key #'car :test #'equal)))

(defun maybe-signal-loom-error (res)
  (let ((status (kv-lookup "status" res)))
    (when (equal status "fail")
      ;; Report error with likely-to-be-location fields removed
      (let ((err (remove-if-not (lambda (key)
                                  (or (eql 0 (search "error_" key))
                                      (equal key "function")
                                      (equal key "action")))
                                res
                                :key #'car)))
        (error 'loom-client-error
               :format-control
               (format nil "Loom error~{, ~a: ~a~}" (mapcan (lambda (x) (list (car x) (cdr x))) err))
               :error-args err)))))

(defun alist-to-kv-string (alist)
  (with-output-to-string (s)
    (format s "(~%")
    (dolist (pair alist)
      (format s ":~a~%=~a~%"
              (downcase-princ-to-string (car pair))
              (downcase-princ-to-string (cdr pair))))
    (format s ")~%")))

(defvar *transaction-stream* nil)
(defparameter *transaction-retry-count* 5)

(defmacro with-loom-transaction ((&key (retry-count-form
                                        '*transaction-retry-count*)
                                       server)
                                 &body body)
  (let* ((thunk (gensym "THUNK"))
         (body `(flet ((,thunk () ,@body))
                  (declare (dynamic-extent #',thunk))
                  (call-with-loom-transaction ,retry-count-form #',thunk))))
    (if server
        `(with-loom-server (,server) ,body)
        body)))

(defun call-with-loom-transaction (retry-count thunk)
  (if *transaction-stream*
      ;; Nested transaction
      (funcall thunk)
      (let ((*transaction-stream* t))
        (dotimes (i retry-count)
          (let ((committed nil))
            (unwind-protect
                 (progn
                   (unless (eq *transaction-stream* t)
                     (ignore-errors (close *transaction-stream*))
                     (setf *transaction-stream* t))
                   (begin-transaction)
                   (block nil               ;(return) below exits here
                     (return-from call-with-loom-transaction
                       (multiple-value-prog1
                           (funcall thunk)
                         (setf committed t)
                         (handler-case
                             (commit-transaction)
                           (error ()
                             ;; Retry
                             (return)))))))
              (unless committed
                (ignore-errors (cancel-transaction))
                (unless (eq *transaction-stream* t)
                  (ignore-errors (close *transaction-stream*))))))))))

(defun initialize-server ()
  "Rewrite the configuration for a local server and restart it."
  (attempt-loom-server-startup t))

;;; ----------------------------------------------------------------------------

(defvar *attempting-loom-server-startup* nil)

(defun load-loom-perl ()
  (let ((code-dir (asdf:system-relative-pathname "cl-loom" "Loom/code/"))
        (dir (asdf:system-relative-pathname "cl-loom" "./")))
    (unless (probe-file code-dir)
      (let ((script (asdf:system-relative-pathname "cl-loom" "git-submodule-init")))
        (asdf:run-shell-command "cd '~a';~a" dir (truename script))))))

(defun attempt-loom-server-startup (&optional (initialize-p t))
  "See https://github.com/billstclair/Loom/wiki/Config"
  (with-server-bound (*loom-server*)
    (assert (config-option 'local (config-of server))
            (server)
            "Expected server ~a to be local.~%~s"
            server (config-of server))
    (let* ((*configuration* (config-of server))
           (config-dir (config-path 'config-dir))
           (binary-pathname (config-path 'binary-path)))
      (when (and config-dir (not (probe-file config-dir)))
        (load-loom-perl))
      (assert (and config-dir (pathnamep config-dir) (probe-file config-dir)
                   binary-pathname (pathnamep binary-pathname)
                   (probe-file binary-pathname))
              (config-dir binary-pathname)
              "Expected config-dir=~a and binary-pathname=~a to be valid paths"
              config-dir binary-pathname)
      ;; Shut down server
      (asdf:run-shell-command "'~a' -n" binary-pathname)
      ;; Write config files
      (let ((config-file (merge-pathnames "sloop" config-dir)))
        (when (or initialize-p (not (probe-file config-file)))
          (with-open-file (s config-file
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
            (write-string (alist-to-kv-string
                           `(("module" . "Loom::Web::Main")
                             ("host_port" . ,(config-option 'port))
                             ("use_error_log" . 1)))
                          s))))
      (let ((loom-file (merge-pathnames "loom" config-dir)))
        (when (or initialize-p (not (probe-file loom-file)))
          (with-open-file (s loom-file
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
            (write-string
             (alist-to-kv-string '(("config_id" . "0123456789abcdef0123456789abcdef")))
             s))))
      ;; start server
      (asdf:run-shell-command "'~a' -y" binary-pathname)
      (sleep 0.5)
      ;; Loom seems to like an initial non-transaction request
      (let ((*attempting-loom-server-startup* t)
            (*transaction-stream* nil))
        (ignore-errors                  ;first transaction gets an error. Don't know why.
          (with-loom-transaction ()
            (sha256 "foo"))
          nil)))))

;; URL path for Loom API HTTP GET request is currently not URL-encoded in its
;; entirety.  Strings should be URL-encoded to protect multi-byte characters and
;; even 8-bit characters.  Absent URL-encoding, only US-ASCII, and some 7-bit
;; European sets, are reliably handled by Web servers. However, it is not clear
;; that Loom expects URL-encoding on all components of the URL path.

;; Solution for the Loom API is to URL-encode strings that are treated as
;; archive content, such as asset descriptions, where there is a real need for
;; multi-byte UTF-8 encoding.

;; Note also, HTTP request method (below) uses UTF-8 for input and output HTTP
;; streams. Oddly - and this may be a bug in the code here or in Drakma - the
;; URL-encoded strings are emitted in HTTP output with the hex coding intact, so
;; the Loom client must URL-decode them - to be investigated.

;; URL-encoding is done for archive content strings (asset descriptions).
;; *URL-ENCODE-LOOM-CLIENT-STRINGS* is a Boolean that enables or disables
;; URL-encoding. Default: ON.

(defvar *url-encode-loom-client-strings* t)

;; *LOOM-CLIENT-DEFAULT-EXTERNAL-FORMAT* is a keyword symbol naming ane external
;; format.  Default is UTF-8. Using LATIN-1 is known to fail, and no other
;; formats have been tested.

(defvar *loom-client-default-external-format* ':utf-8)

(defun url-encode-loom-client-string (str &optional (external-format *loom-client-default-external-format*))
  (if (and *url-encode-loom-client-strings*
           (find-if (lambda (char) (>= (char-code char) 128)) str))
      (map 'string #'code-char
           (flexi-streams:string-to-octets
            str :external-format external-format))
      str))

(defun url-decode-loom-client-string (str &optional (external-format *loom-client-default-external-format*))
  (if *url-encode-loom-client-strings*
      (flexi-streams:octets-to-string
       (map 'vector #'char-code str)
       :external-format external-format)
      str))

(defun request (path &rest args)
  (with-server-bound ()
    (let* ((stringified-args (mapcar #'downcase-princ-to-string args))
           (uri (base-uri-of server)))
      (when path
        (setf uri (concatenate 'string uri path)))
      (flet ((do-request ()
               (let* ((chunga:*accept-bogus-eols* t) ; #\return or #\newline instead of both
                      (stream *transaction-stream*)
                      (keep-alive-p (not (null stream))))
                 (ssl-verify-init-with-rayservers-ca-certificate)
                 (multiple-value-bind (res status headers uri http-stream)
                     (drakma:http-request uri
                                          :method :POST
                                          :parameters (alexandria:plist-alist stringified-args)
                                          :external-format-out ':latin-1
                                          :external-format-in ':latin-1
                                          :keep-alive keep-alive-p
                                          :close (not keep-alive-p)
                                          :stream (and (not (eq stream t)) stream))
                   (declare (ignore status headers uri))
                   (when keep-alive-p
                     (setf *transaction-stream* http-stream))
                   res))))
        (parse-response
         (handler-bind
             ((usocket:connection-refused-error
               (lambda (c)
                 (declare (ignore c))
                 (unless *attempting-loom-server-startup*
                   (attempt-loom-server-startup t)
                   (return-from request (do-request))))))
           (do-request)))))))

(defun begin-transaction ()
  (request "trans/begin"))

(defun commit-transaction ()
  (request "trans/commit"))

(defun cancel-transaction ()
  (request "trans/cancel"))

(defun grid-request (op &rest args)
  (apply #'request "grid" :action op args))

(deftype loom-loc ()
  '(string 32))

(deftype loom-hash ()
  '(string 64))

(defun grid-buy (asset-type location usage &optional ignore-if-occupied-p)
  "Buy storage for ASSET-TYPE at LOCATION. Pay for it with one token from USAGE.
Don't error if occupid if IGNORE-IF-OCCUPIED-P is true."
  (check-type asset-type loom-loc)
  (check-type location loom-loc)
  (check-type usage loom-loc)
  (let* ((*skip-response-error-check*
          (or ignore-if-occupied-p *skip-response-error-check*))
         (res (grid-request :buy :type asset-type :loc location :usage usage)))
    (when ignore-if-occupied-p
      (when (and (equal "fail" (kv-lookup "status" res))
                 (not (equal "occupied" (kv-lookup "error_loc" res))))
        (maybe-signal-loom-error res)))
    res))

(defun grid-sell (asset-type location usage &optional ignore-if-vacant-p)
  "Sell storage for ASSET-TYPE at LOCATION. Credit USAGE with one token.
Error if the location is vacant, unless IGNORE-IF-VACANT-P is true."
  (check-type asset-type loom-loc)
  (check-type location loom-loc)
  (check-type usage loom-loc)
  (let* ((*skip-response-error-check*
          (or ignore-if-vacant-p *skip-response-error-check*))
         (res (grid-request :sell :type asset-type :loc location :usage usage)))
    (when ignore-if-vacant-p
      (when (and (equal "fail" (kv-lookup "status" res))
                 (not (equal "vacant" (kv-lookup "error_loc" res))))
        (maybe-signal-loom-error res)))
    res))

(defparameter *zero*
  (make-string 32 :initial-element #\0))

(defparameter *one*
  (concatenate 'string (make-string 31 :initial-element #\0) "1"))

(defun grid-issuer (asset-type orig dest)
  "Change the issuer location for ASSET-TYPE from ORIG to DEST.
To create a new asset, move it from *ZERO*. To destroy an asset with an issuer
balance of 0 (-1), move it to *ZERO*."
  (check-type asset-type loom-loc)
  (check-type orig loom-loc)
  (check-type dest loom-loc)
  (grid-request :issuer :type asset-type :orig orig :dest dest))

(defun adjust-negative (val &optional raw-negatives-p)
  (if (or raw-negatives-p (>= val 0))
      val
      (1+ val)))

(defun parse-value (string &optional raw-negatives-p)
  (adjust-negative (parse-integer string) raw-negatives-p))

(defun grid-touch (asset-type location &optional no-error-p raw-negatives-p)
  "Query the value of ASSET-TYPE at LOCATION.
If vacant, signal an error unless ZERO-IF-VACANT-P is true.
If raw-negatives-p is true, don't increment by one negative values.
Returns two values:
  1) The value, an integer
  2) An alist of the full Loom return"
  (check-type asset-type loom-loc)
  (check-type location loom-loc)
  (let* ((*skip-response-error-check*
          (or no-error-p *skip-response-error-check*))
         (res (grid-request :touch :type asset-type :loc location)))
    (cond ((and no-error-p
                (equal "fail" (kv-lookup "status" res)))
           (unless (equal "vacant" (kv-lookup "error_loc" res))
             (maybe-signal-loom-error res))
           ;; Return -1 for issuer locations when raw-negatives-p is true
           (cond ((string= *zero* location) (values (if raw-negatives-p -1 0) res))
                 (t (values 0 res))))
          (t (values
              (parse-value (kv-lookup "value" res) raw-negatives-p)
              res)))))

(defun grid-vacant-p (asset-type location)
  "Return true if the given grid LOCATION is vacant for ASSET-TYPE"
  (check-type asset-type loom-loc)
  (check-type location loom-loc)
  (let* ((*skip-response-error-check* t)
         (res (grid-request :touch :type asset-type :loc location)))
    (and (equal "fail" (kv-lookup "status" res))
         (equal "vacant" (kv-lookup "error_loc" res)))))

(defun random-vacant-asset-type ()
  (loop for loc = (random-loc)
     when (= (grid-touch loc *zero* t t) -1)
     return loc))

(defun random-vacant-grid-loc (asset-type)
  (loop for loc = (random-loc)
     when (grid-vacant-p asset-type loc)
     return loc))

(defun grid-look (asset-type hash &optional raw-negatives-p)
  "Query the value of ASSET-TYPE at HASH.
Returns two values:
  1) The value, an integer
  2) An alist of the full Loom return
If RAW-NEGATIVES-P is true, don't adjust a negative value by adding 1."
  (check-type asset-type loom-loc)
  (check-type hash loom-hash)
  (let ((res (grid-request :look :type asset-type :hash hash)))
    (values
     (parse-value (kv-lookup "value" res) raw-negatives-p)
     res)))

(defun grid-move (asset-type qty from to &optional raw-negatives-p)
  "Move QTY units of ASSET-TYPE from FROM to TO.
Return three values, the new balance at FROM, the new balance at TO,
and the returned alist from the server.
If RAW-NEGATIVES-P is true, don't adjust negative values by adding 1"
  (check-type asset-type loom-loc)
  (check-type qty number)
  (check-type from loom-loc)
  (check-type to loom-loc)
  (let ((kv (grid-request :move :type asset-type :qty qty :orig from :dest to)))
    (values
     (parse-value (kv-lookup :value_orig kv) raw-negatives-p)
     (parse-value (kv-lookup :value_dest kv) raw-negatives-p)
     kv)))     

;; Still need to parse (("loc/L" . "qty:id qty:id ...") ...) pairs and return
;; an alist: ((L (id . qty) (id . qty)) ...)
(defun grid-scan (locs types &optional zeroes-p)
  "Scans all the locations in the LOCS list for all the asset-types  in the TYPES list.
Returns zero values if zeroes-p is included and true.
Result isn't yet parsed. Do that when you need this function."
  (check-type locs cons)
  (check-type types cons)
  (let ((locs-string (format nil "~a~{ ~a~}" (car locs) (cdr locs)))
        (types-string (format nil "~a~{ ~a~}" (car types) (cdr types))))
    (if zeroes-p
        (grid-request :scan :locs locs-string :types types-string :zeroes 1)
        (grid-request :scan :locs locs-string :types types-string))))

(defun create-asset (asset-type issuer-loc &optional (usage issuer-loc))
  "Create a new asset of type ASSET-TYPE at ISSUER-LOC.
Use usage tokens from USAGE, default: ISSUER-LOC."
  (let* ((type (if (typep asset-type 'loom-loc)
                   asset-type (random-vacant-asset-type)))
         (issuer (if (typep issuer-loc 'loom-loc)
                     issuer-loc (random-vacant-grid-loc type))))
    (grid-buy type issuer usage t)
    (grid-buy type *zero* usage t)
    (grid-issuer type *zero* issuer)
    (grid-sell type *zero* usage)
    (values type issuer)))

(defun destroy-asset (asset-type issuer-loc &optional (usage issuer-loc))
  (grid-buy asset-type *zero* usage t)
  (grid-issuer asset-type issuer-loc *zero*)
  (grid-sell asset-type issuer-loc usage)
  (grid-sell asset-type *zero* usage))

(defun archive-request (op &rest args)
  (apply #'request "archive" :action op args))

(defun archive-buy (location usage &optional ignore-if-occupied-p)
  "Buy grid storage at LOCATION. Pay for it with one token from USAGE
Don't error if occupid if occupied and IGNORE-IF-OCCUPIED-P is true."
  (check-type location loom-loc)
  (check-type usage loom-loc)
  (let* ((*skip-response-error-check*
          (or ignore-if-occupied-p *skip-response-error-check*))
         (res (archive-request :buy :loc location :usage usage)))
    (when ignore-if-occupied-p
      (when (and (equal "fail" (kv-lookup "status" res))
                 (not (equal "occupied" (kv-lookup "error_loc" res))))
        (maybe-signal-loom-error res)))
    res))

(defun archive-sell (location usage &optional ignore-if-vacant-p)
  "Sell grid storage at LOCATION. Credit USAGE with one token.
Error if the location is vacant, unless IGNORE-IF-VACANT-P is true."
  (check-type location loom-loc)
  (check-type usage loom-loc)
  (let* ((*skip-response-error-check*
          (or ignore-if-vacant-p *skip-response-error-check*))
         (res (archive-request :sell :loc location :usage usage)))
    (when ignore-if-vacant-p
      (when (and (equal "fail" (kv-lookup "status" res))
                 (not (equal "vacant" (kv-lookup "error_loc" res))))
        (maybe-signal-loom-error res)))
    res))


(defun archive-touch (location)
  "Query the archive value of at LOCATION.
Returns three values:
  1) The value, a string
  2) The hash of the value, a string
  2) An alist of the full Loom return"
  (check-type location loom-loc)
  (let ((res (archive-request :touch :loc location)))
    (values (aif (kv-lookup "content" res) (url-decode-loom-client-string it))
            (kv-lookup "content_hash" res)
            res)))

(defun archive-vacant-p (location)
  "Return true if the given archive LOCATION is vacant"
  (check-type location loom-loc)
  (let* ((*skip-response-error-check* t)
         (res (archive-request :touch :loc location)))
    (and (equal "fail" (kv-lookup "status" res))
         (equal "vacant" (kv-lookup "error_loc" res)))))

(defun random-vacant-archive-loc ()
  (loop for loc = (random-loc)
     when (archive-vacant-p loc)
     return loc))

(defun archive-look (hash)
  "Query the archive value of at HASH.
Returns three values:
  1) The value, a string
  2) The hash of the value, a string
  2) An alist of the full Loom return"
  (check-type hash loom-hash)
  (let ((res (archive-request :look :hash hash)))
    (values (aif (kv-lookup "content" res) (url-decode-loom-client-string it))
            (kv-lookup "content_hash" res)
            res)))

(defun archive-write (location content usage &optional guard)
  "Write CONTENT into the archive at LOCATION. Debit or credit tokens from USAGE.
If GUARD is included, it should be the hash of the former value, as returned as the
second value from ARCHIVE-TOUCH or ARCHIVE-LOOK."
  (check-type location loom-loc)
  (check-type content string)
  (check-type usage loom-loc)
  (check-type guard (or null loom-hash))
  ;; 
  (let* ((ucontent (url-encode-loom-client-string content))
         (res (if guard
                  (archive-request
                   :write :loc location :content ucontent :usage usage :guard guard)
                  (archive-request
                   :write :loc location :content ucontent :usage usage))))
    (values (kv-lookup "content_hash" res)
            res)))

(defun random-loc ()
  "Return a random LOOM location, as a 32-character hex number."
  ;;(kv-lookup "value" (request "random"))
  (string-downcase (format nil "~32,'0x" (cl-crypto:get-random-bits 128))))

(defvar *sha256-function* nil)

(defun sha256-function ()
  "Return the function that computes the sha256 hash of a string."
  (or *sha256-function* 'sha256))

(defun (setf sha256-function) (function)
  "Set a function of one arg, a string, which returns the sha256 hash of that string."
  (setf *sha256-function* function))

(defun sha256 (string)
  "Returns two values, the sha256 hash of STRING, a 256-bit number represented as
64 hex characters, and the folding of that into a 128-bit Loom ID, as if by FOLD-HASH.
Requires *loom-server* to be bound, unless you set a local function with (setf sha256-function)."
  (check-type string string)
  (if (or (null *sha256-function*)
          (eq 'sha256-internal *sha256-function*))
      (sha256-internal string)
      (let ((res (funcall *sha256-function* string)))
        (values res (fold-hash res)))))

(defun sha256-internal (string)
  (let ((res (request "hash" :input string)))
    (values (kv-lookup "sha256_hash" res)
            (kv-lookup "folded_hash" res)
            res)))

(defun location-to-binary-string (location &optional (str (make-string 16)) (start 0))
  (check-type location loom-loc)
  (check-type str string)
  (check-type start integer)
  (let ((pos 0))
    (dotimes (i 16)
      (setf (aref str (+ i start))
            (code-char (parse-integer (subseq location pos (+ pos 2)) :radix 16)))
      (incf pos 2))
    str))

(defun location-to-uint-8-array (location &optional
                                 (array (make-array
                                         16 :element-type '(unsigned-byte 8)))
                                 (start 0))
  (check-type location loom-loc)
  (check-type array (array (unsigned-byte 8)))
  (check-type start integer)
  (let ((pos 0))
    (dotimes (i 16)
      (setf (aref array (+ i start))
            (parse-integer (subseq location pos (+ pos 2)) :radix 16))
      (incf pos 2))
    array))

(defun grid-hash (asset-type location)
  "Return the hash that can be passed to grid-look to read the value of
ASSET-TYPE at LOCATION. Return the hash'es folded location as a second value.
Currently requires *loom-server* to be bound."
  (check-type asset-type loom-loc)
  (check-type location loom-loc)
  (let ((str (make-string 32)))
    (location-to-binary-string asset-type str)
    (location-to-binary-string location str 16)
    (sha256 str)))

(defun archive-hash (location)
  "Return the hash that can be passed to asset-look to read the value
at LOCATION. Return the hash'es folded location as a second value.
Currently requires *loom-server* to be bound."
  (check-type location loom-loc)
  (sha256 (location-to-binary-string location)))

(defun format-loom-loc (integer)
  (check-type integer integer)
  (format nil "~(~32,'0x~)" integer))

(defun loom-loc-xor (&rest ids)
  (let ((res 0))
    (dolist (id ids)
      (check-type id loom-loc)
      (setf res (logxor res (parse-integer id :radix 16))))
    (format-loom-loc res)))

(defun fold-hash (hash)
  (check-type hash loom-hash)
  (loom-loc-xor (subseq hash 0 32) (subseq hash 32 64)))

(defun string-to-hex (string)
  (apply #'concatenate
         'string
         (map 'list
              ;; Lower-case important ~(...~). Loom doesn't recognize upper case hex digits
              (lambda (char) (format nil "~(~2,'0x~)" (char-code char)))
              string)))

(defun hex-to-string (hex)
  (let* ((len (length hex))
         (res (make-string (or (and (evenp len) (/ len 2))
                               (error "Odd-length hex string")))))
    (loop for i from 0 below len by 2
       for j from 0
       for code = (parse-integer hex :start i :end (+ i 2) :radix 16.)
       do (setf (aref res j) (code-char code)))
    res))

;; From loom source: code/Loom/Web/Page_Asset.pm
(defun make-asset-description (name asset-id scale precision)
  (when (null scale) (setf scale ""))
  (when (null precision) (setf precision ""))
  (let ((sans-hash (format nil "~ax~dx~dx~a" asset-id scale precision
                           (string-to-hex name))))
    (concatenate 'string
                 sans-hash
                 "x"
                 (subseq (loom:sha256 sans-hash) 0 8))))

(defun parse-asset-description (description)
  "Returns 4 values: name, asset-id, scale, and precision"
  (destructuring-bind (asset-id scale precision hex-name hash)
      (split-sequence:split-sequence #\x description)
    (let* ((sans-hash (format nil "~ax~ax~ax~a" asset-id scale precision hex-name))
           (computed-hash (subseq (loom:sha256 sans-hash) 0 8)))
      (unless (equal computed-hash hash)
        (error "Hash mismatch"))
      (values (hex-to-string hex-name)
              asset-id
              (unless (equal scale "") (parse-integer scale))
              (unless (equal precision "") (parse-integer precision))))))

;; The contents of a Loom Folder, pruned to one example of each type
#||
Content-type: loom/folder

(
:list_loc
=a0e8083ff67986a6c20b098cba2fe7fa 00000000000000000000000000000000 00000000000000000000000000000001
:loc_name.a0e8083ff67986a6c20b098cba2fe7fa
=Zippy Admin Wallet
:list_type
=00000000000000000000000000000000 5319c540ec8b46d8f1ad1163fb764a26 596f626574697420476c6f62616c7321
:type_name.00000000000000000000000000000000
=usage tokens
:type_name.5319c540ec8b46d8f1ad1163fb764a26
=2011/2/6, 19:30EST, AH -2.5, Green Bay Packers win vs. Pittsburg Steelers
:type_scale.5319c540ec8b46d8f1ad1163fb764a26
=6
:type_min_precision.5319c540ec8b46d8f1ad1163fb764a26
=2
:recording
=1
:list_H
=44bfd073 8f0b5ae6 
:H_time.8f0b5ae6
=1296164583
:H_qty.8f0b5ae6
=-10000000
:H_type.8f0b5ae6
=5319c540ec8b46d8f1ad1163fb764a26
:H_loc.8f0b5ae6
=00000000000000000000000000000001
:H_memo.8f0b5ae6
=This is a note.
)
||#

;; Elements of a Loom wallet
(defstruct asset
  name
  scale
  precision
  id
  disabled-p)

(defstruct location
  name
  loc
  disabled-p
  wallet-p)

(defstruct history
  hash
  time
  qty
  type
  loc
  memo)

;; The wallet itself
(defstruct wallet
  assets
  locations
  recording-p
  history
  properties)

(defun wallet-get-property (wallet property)
  (check-type wallet wallet)
  (setf property (downcase-princ-to-string property))
  (cdr (assoc property (wallet-properties wallet) :test #'equal)))

(defun (setf wallet-get-property) (value wallet property)
  (check-type value (or null string))
  (check-type wallet wallet)
  (setf property (downcase-princ-to-string property))
  (let ((cell (assoc property (wallet-properties wallet) :test #'equal)))
    (if cell
        (if (null value)
            (setf (wallet-properties wallet)
                  (delete cell (wallet-properties wallet) :test #'eq))
            (setf (cdr cell) value))
        (push (cons property value) (wallet-properties wallet)))))

(defun find-asset (name asset-list &optional return-id-p)
  "Find the asset named NAME in ASSET-LIST.
Return the asset if RETURN-ID-P is false, or its id if true.
If ASSET-LIST is a WALLET instance, search its WALLET-ASSETS."
  (when (typep asset-list 'wallet)
    (setf asset-list (wallet-assets asset-list)))
  (check-type name string)
  (check-type asset-list list)
  (let ((asset (find name asset-list :test #'equal :key #'asset-name)))
    (when asset
      (if return-id-p
          (asset-id asset)
          asset))))

(defun find-asset-by-id (id asset-list &optional return-name-p)
  "Find the asset with ID in ASSET-LIST.
Return the asset if RETURN-NAME-P is false, or its NAME if true.
If ASSET-LIST is a WALLET instance, search its WALLET-ASSETS."
  (when (typep asset-list 'wallet)
    (setf asset-list (wallet-assets asset-list)))
  (check-type id string)
  (check-type asset-list list)
  (let ((asset (find id asset-list :test #'equal :key #'asset-id)))
    (when asset
      (if return-name-p
          (asset-name asset)
          asset))))

(defun find-location (name location-list &optional return-loc-p)
  "Find the location named NAME in LOCATION-LIST.
Return the location if return-loc-p is false, or its loc if true.
If LOCATION-LIST is a WALLET instance, search its WALLET-LOCATIONS."
  (when (typep location-list 'wallet)
    (setf location-list (wallet-locations location-list)))
  (check-type name string)
  (check-type location-list list)
  (let ((location (find name location-list :test #'equal :key #'location-name)))
    (when location
      (if return-loc-p
          (location-loc location)
          location))))

(defun find-location-by-loc (loc location-list &optional return-name-p)
  "Find the location with a loc of LOC in LOCATION-LIST.
Return the location if return-name-p is false, or its name if true.
If LOCATION-LIST is a WALLET instance, search its WALLET-LOCATIONS."
  (when (typep location-list 'wallet)
    (setf location-list (wallet-locations location-list)))
  (check-type loc loom-loc)
  (check-type location-list list)
  (let ((location (find loc location-list :test #'equal :key #'location-loc)))
    (when location
      (if return-name-p
          (location-name location)
          location))))

(defparameter *loom-folder-header*
  (concatenate
   'string
   "Content-type: loom/folder"
   (string #\newline)
   (string #\newline)))

;; Return an alist of all the unhandled wallet properties in alist
(defun filter-wallet-properties (alist)
  (loop for (key . value) in alist
       unless (or (member key '("list_loc" "list_type" "list_H" "recording")
                          :test #'equal)
                  (eql 0 (search "loc_name." key :test #'equal))
                  (eql 0 (search "loc_disable." key :test #'equal))
                  (eql 0 (search "type_name." key :test #'equal))
                  (eql 0 (search "type_scale." key :test #'equal))
                  (eql 0 (search "type_min_precision." key :test #'equal))
                  (eql 0 (search "type_disable." key :test #'equal))
                  (eql 0 (search "H_time." key :test #'equal))
                  (eql 0 (search "H_qty." key :test #'equal))
                  (eql 0 (search "H_loc." key :test #'equal))
                  (eql 0 (search "H_memo." key :test #'equal)))
       collect (cons key value)))

(defun parse-wallet-string (string)
  (unless (eql 0 (search *loom-folder-header* string :test #'string-equal))
    (error "Missing loom folder header in: ~s" string))
  (let* ((alist (parse-kv (subseq string (length *loom-folder-header*))))
         (locs (split-sequence:split-sequence #\space (kv-lookup "list_loc" alist)
                                              :remove-empty-subseqs t))
         (types (split-sequence:split-sequence #\space (kv-lookup "list_type" alist)
                                               :remove-empty-subseqs t))
         (recording (kv-lookup "recording" alist))
         (hs (split-sequence:split-sequence #\space (kv-lookup "list_H" alist)
                                            :remove-empty-subseqs t))
         (properties (filter-wallet-properties alist)))
    (make-wallet
     :locations
     (loop for loc in locs
        collect
          (make-location :name (kv-lookup (format nil "loc_name.~a" loc) alist)
                         :loc loc
                         :disabled-p (not (null (kv-lookup
                                                 (format nil "loc_disable.~a" loc)
                                                 alist)))
                         :wallet-p (equal loc (car locs))))
     :assets
     (loop for id in types
        collect
          (make-asset :name (kv-lookup (format nil "type_name.~a" id) alist)
                      :scale (awhen (kv-lookup (format nil "type_scale.~a" id) alist)
                               (parse-integer it))
                      :precision (awhen (kv-lookup (format nil "type_min_precision.~a" id)
                                                   alist)
                                   (parse-integer it))
                      :id id
                      :disabled-p (not (null (kv-lookup
                                              (format nil "type_disable.~a" id)
                                              alist)))))
     :recording-p (equal recording "1")
     :history
     (loop for hash in hs
        collect
          (make-history
           :hash hash
           :time (kv-lookup (format nil "H_time.~a" hash) alist)
           :qty (kv-lookup (format nil "H_qty.~a" hash) alist)
           :type (kv-lookup (format nil "H_type.~a" hash) alist)
           :loc (kv-lookup (format nil "H_loc.~a" hash) alist)
           :memo (kv-lookup (format nil "H_memo.~a" hash) alist)))
     :properties properties)))

(defun wallet-string (wallet)
  (check-type wallet wallet)
  (let ((alist nil))
    (macrolet ((listit (idf list name)
                 `(let ((,list (mapcar ,idf ,list)))
                    (push (cons ,name (format nil "~a~{ ~a~}" (car ,list) (cdr ,list)))
                          alist)))
               (pushit (it fmt arg)
                 `(awhen ,it
                    (push (cons (format nil ,fmt ,arg) it) alist))))
      (awhen (wallet-locations wallet)
        (listit #'location-loc it "list_loc")
        (dolist (location it)
          (let ((loc (location-loc location)))
            (pushit (location-name location) "loc_name.~a" loc)
            (when (location-disabled-p location)
              (pushit "1" "loc_disable.~a" loc)))))
      (awhen (wallet-assets wallet)
        (listit #'asset-id it "list_type")
        (dolist (asset it)
          (let ((id (asset-id asset)))
            (pushit (asset-name asset) "type_name.~a" id)
            (pushit (asset-scale asset) "type_scale.~a" id)
            (pushit (asset-precision asset) "type_min_precision.~a" id)
            (when (asset-disabled-p asset)
              (pushit "1" "type_disable.~a" id)))))
      (when (wallet-recording-p wallet)
        (push (cons "recording" "1") alist))
      (awhen (wallet-history wallet)
        (listit #'history-hash it "list_H")
        (dolist (history it)
          (let ((hash (history-hash history)))
            (pushit (history-time history) "H_time.~a" hash)
            (pushit (history-qty history) "H_qty.~a" hash)
            (pushit (history-type history) "H_type.~a" hash)
            (pushit (history-loc history) "H_loc.~a" hash)
            (pushit (history-memo history) "H_memo.~a" hash))))
      (loop for (key . value) in (wallet-properties wallet)
           do (push (cons key value) alist))
      (concatenate 'string *loom-folder-header* (alist-to-kv-string (nreverse alist))))))

(defun loom-passphrase-p (string)
  (and (stringp string)
       (>= (length string) 8)))

(deftype loom-passphrase ()
  '(satisfies loom-passphrase-p))

(defun passphrase-location (location &optional is-passphrase-p private-p)
  (let ((res (cond (is-passphrase-p
                    (check-type location loom-passphrase)
                    (nth-value 1 (sha256 location)))
                   (t
                    (check-type location loom-loc)
                    location))))
    (if private-p
        (values (nth-value 1 (sha256 res)) res)
        res)))

(defun get-wallet (location &optional is-passphrase-p usage private-p)
  "Fetch, parse, and return, as a LOOM:WALLET instance, the loom wallet for LOCATION.
If IS-PASSPHRASE-P is true, LOCATION is a passphrase, not a location.
USAGE ignored here, but used by setf method."
  (when private-p
    (return-from get-wallet (get-private-wallet location is-passphrase-p usage)))
  (setf location (passphrase-location location is-passphrase-p))
  (values (parse-wallet-string (archive-touch location)) location))

(defun (setf get-wallet) (wallet location &optional is-passphrase-p usage private-p)
  "Save WALLET, a LOOM-WALLET:WALLET instance, to LOCATION on *LOOM-SERVER*.
If IS-PASSPHRASE-P is true, LOCATION is a passphrase, not a location.
USAGE is the location to use for debiting or crediting usage tokens.
Defaults to LOCATION."
  (check-type wallet wallet)
  (when private-p
    (return-from get-wallet
      (setf (get-private-wallet location is-passphrase-p usage) wallet)))
  (setf location (passphrase-location location is-passphrase-p))
  (unless usage (setf usage location))
  (archive-write location (wallet-string wallet) usage)
  (values wallet location))

(defun encrypt-string-for-location (string location)
  (check-type string string)
  (check-type location loom-loc)
  (let* ((passphrase (location-to-uint-8-array location)))
    (multiple-value-bind (res iv)
        (cl-crypto:aes-encrypt-string string passphrase)
      (concatenate 'string (cl-base64:usb8-array-to-base64-string iv) "|" res))))

(defun encrypt-wallet-for-location (wallet location)
  (check-type wallet wallet)
  (check-type location loom-loc)
  (encrypt-string-for-location (wallet-string wallet) location))

(defun base64-to-array (base64-string)
  (handler-bind
      ;; Turn bad character warning into error
      ((warning (lambda (c) (error (princ-to-string c)))))
    (cl-base64:base64-string-to-usb8-array base64-string)))

(defun decrypt-string-from-location (string location)
  (check-type string string)
  (check-type location loom-loc)
  (let* ((passphrase (location-to-uint-8-array location))
         (iv-and-value (split-sequence:split-sequence #\| string))
         (iv (base64-to-array (first iv-and-value))))
    (cl-crypto:aes-decrypt-to-string
     (second iv-and-value) passphrase :iv iv)))

(defun decrypt-wallet-from-location (wallet-string location)
  (parse-wallet-string (decrypt-string-from-location wallet-string location)))

(defun get-private-wallet (location &optional is-passphrase-p usage)
  "Fetch, parse, and return, as a LOOM:WALLET instance, the private loom wallet
for LOCATION, as saved by (SETF GET-PRIVATE-WALLET)
If IS-PASSPHRASE-P is true, LOCATION is a passphrase, not a location.
USAGE ignored here, but used by setf method."
  (declare (ignore usage))
  (multiple-value-bind (hashed-location location)
      (passphrase-location location is-passphrase-p t)
    (let* ((value (archive-touch hashed-location)))
      (values (decrypt-wallet-from-location value location) hashed-location))))

(defun (setf get-private-wallet) (wallet location &optional is-passphrase-p usage)
  "Save WALLET, a LOOM-WALLET:WALLET instance, encrypted with LOCATION,
to (SHA256 LOCATION) on *LOOM-SERVER*. If IS-PASSPHRASE-P is true,
LOCATION is a passphrase, not a location.
USAGE is the location to use for debiting or crediting usage tokens.
Defaults to LOCATION."
  (check-type wallet wallet)
  (setf location (passphrase-location location is-passphrase-p))
  (let ((hashed-location (nth-value 1 (sha256 location)))
        (value (encrypt-wallet-for-location wallet location)))
    (unless usage (setf usage hashed-location))
    (archive-write hashed-location value usage)
    (values wallet hashed-location)))

(defun create-wallet (passphrase usage &key
                      (name "My Wallet")
                      (sponsor-name "My Sponsor")
                      assets
                      locations
                      usage-source
                      (usage-qty 100)
                      private-p)
  "Create a new wallet on *LOOM-SERVER* for PASSPHRASE.
Error if there are already tokens or an archive value at that location.
If USAGE-SOURCE, is specified, tranfer USAGE-QTY tokens to USAGE.
After the transfer, USAGE must contain at least 100 tokens.
Name the new wallet NAME.
Name the USAGE location in the new wallet SPONSOR-NAME.
Add the usage token asset and and others in ASSETS, a list of LOOM:ASSET instances.
If sponsor-name is non-NIL, add USAGE to the wallet with that name.
If usage is the token issuer location, don't put any tokens at the new location.
This is used for creating the token issuer account.
If PRIVATE-P is true, stores the wallet at the hash of the location,
and encrypts it with the location as passphrase.
Returns the location of the new wallet.
If PRIVATE-P is true, the return value will be the hashed location, and
the location itself will be returned as a second value."
  (check-type passphrase string)
  (check-type usage loom-loc)
  (check-type name string)
  (check-type sponsor-name (or null string))
  (check-type usage-source (or null loom-loc))
  (check-type usage-qty integer)

  (with-loom-transaction ()
    (let* ((location (passphrase-location passphrase t))
           (real-location (if private-p (nth-value 1 (sha256 location)) location))
           (session (loom-loc-xor location *one*))
           (already-tokens (grid-touch *zero* usage t t))
           (tokens (if usage-source usage-qty already-tokens)))
      (assert (or (< already-tokens 0) (>= tokens 100)) nil
              "Must provide at least 100 usage tokens to new account.")
      (unless (and (or (equal real-location usage)
                       (grid-vacant-p *zero* real-location))
                   (archive-vacant-p real-location))
        (error "Passphrase location already in use."))
      (when usage-source
        (unless (> already-tokens 0)
          (grid-buy *zero* usage usage-source t)
          (unless (>= already-tokens usage-qty)
            (grid-move *zero* (- usage-qty already-tokens) usage-source usage))))
      (unless private-p
        (archive-buy session usage t)
        ;; Can't log in without a session, stored in the archive
        ;; The session value is written at the wallet location xor 1
        ;; The wallet location is written at the session value
        (let ((session-value (random-loc)))
          (archive-write session session-value usage)
          (archive-buy session-value usage t)
          (archive-write session-value location usage)))
      (archive-buy real-location usage t)
      (when sponsor-name
        (push (make-location :name sponsor-name :loc usage)
              locations))
      (let ((wallet (make-wallet
                     :assets
                     (cons (make-asset :name "usage tokens" :id *zero*) assets)
                     :locations
                     (cons (make-location :name name :loc real-location)
                           locations)
                     :recording-p t)))
        (setf (get-wallet location nil usage private-p) wallet))
      (unless (< already-tokens 0)
        (grid-buy *zero* real-location usage t)
        (grid-move *zero* (grid-touch *zero* usage) usage real-location))
      real-location))) 

(defun move-wallet (location new-passphrase &key
                    location-is-passphrase-p
                    old-private-p
                    new-private-p)
  "Move a Loom wallet from LOCATION on *LOOM-SERVER* to NEW-PASSPHRASE.
LOCATION is a passphrase if LOCATION-IS-PASSPHRASE-P is true.
If OLD-PRIVATE-P is true, the old wallet is private.
If NEW-PRIVATE-P is true, the new wallet will be private."
  (check-type location string)
  (check-type new-passphrase string)
  (with-loom-transaction ()
    (setf location (passphrase-location location location-is-passphrase-p))
    (let* ((real-location (if old-private-p
                              (nth-value 1 (sha256 location))
                              location))
           (wallet (get-wallet location nil nil old-private-p))
           (my-location (find real-location (wallet-locations wallet)
                              :test #'equal
                              :key #'location-loc))
           (asset-ids (delete *zero* (mapcar #'asset-id (wallet-assets wallet))
                              :test #'equal))
           (session (unless old-private-p (loom-loc-xor location *one*)))
           (session-value (and session (ignore-errors (archive-touch session))))
           (new-location (passphrase-location new-passphrase t))
           (real-new-location (if new-private-p
                                  (nth-value 1 (sha256 new-location))
                                  new-location))
           (new-session (unless new-private-p (loom-loc-xor new-location *one*))))
      (grid-touch *zero* real-location)
      (unless (and (grid-vacant-p *zero* real-new-location)
                   (archive-vacant-p real-new-location))
        (error "New passphrase location already in use."))
      (unless my-location
        (error "Can't find wallet name."))
      (setf (location-loc my-location) real-new-location)
      (unless old-private-p
        (when session-value
          (archive-touch session-value))
        (archive-write session "" real-location)
        (archive-sell session real-location))
      (archive-write real-location "" real-location)
      (archive-sell real-location real-location)
      (dolist (asset-id asset-ids)
        (let ((value (grid-touch asset-id real-location t)))
          (unless (zerop value)
            (grid-buy asset-id real-new-location real-location)
            (grid-move asset-id value real-location real-new-location)
            (grid-sell asset-id real-location real-location))))
      (grid-buy *zero* real-new-location real-location)
      (grid-move *zero* (grid-touch *zero* real-location)
                 real-location real-new-location)
      (grid-sell *zero* real-location real-new-location)
      (archive-buy real-new-location real-new-location)
      (setf (get-wallet new-location nil nil new-private-p) wallet)
      (when new-session
        (archive-buy new-session new-location)
        (when session-value
          (archive-write new-session session-value new-location)
          (archive-write session-value new-location new-location)))
      real-new-location)))

;; Delete the wallet at LOCATION/LOCATION-IS-PASSPHRASE-P and its session storage.
;; Credit the usage tokens to USAGE.
;; Wallet is private if private-p is true.
(defun delete-wallet (location usage &key
                      location-is-passphrase-p
                      usage-is-passphrase-p
                      private-p
                      usage-private-p)
  (check-type location string)
  (check-type usage string)
  (with-loom-transaction ()
    (setf location (passphrase-location location location-is-passphrase-p)
          usage (passphrase-location usage usage-is-passphrase-p))
    (let* ((real-location (if private-p
                              (nth-value 1 (sha256 location))
                              location))
           (real-usage (if usage-private-p
                           (nth-value 1 (sha256 usage))
                           usage))
           (session (unless private-p (loom-loc-xor location *one*)))
           (session-value (and session (ignore-errors (archive-touch session)))))
      (unless private-p
        (when session-value
          (archive-write session-value "" real-usage)
          (archive-sell session-value real-usage))
        (archive-write session "" real-usage)
        (archive-sell session real-usage))
      (archive-write real-location "" real-usage)
      (archive-sell real-location real-usage)
      t)))

;; Scans all active locations and types in wallet, and returns an alist:
;; ((location (id . qty) (id . qty) ...) ...)
;; The QTY values are all shifted as specified by the wallet locations'
;; scales and precisions.
;; To do: Handle greater that 2048 combinations, Loom's max for one call.
;; If wallet is NIL, then locations should be a list of raw loom-loc strings,
;; and assets should be a list of raw asset id strings.
(defun grid-scan-wallet (wallet &key
                         (locations (wallet-locations wallet) locations-p)
                         (assets (wallet-assets wallet) assets-p))
  (check-type wallet (or null wallet))
  (let* ((locs (loop for loc in locations
                  unless (and wallet (not locations-p) (location-disabled-p loc))
                  collect (if wallet (location-loc loc) loc)))
         (types (loop for asset in assets
                   unless (and wallet (not assets-p) (asset-disabled-p asset))
                   collect (if wallet (asset-id asset) asset)))
         (alist (grid-scan locs types)))
    (loop for (key . values) in alist
       when (eql 0 (search "loc/" key :test #'string-equal))
       collect
         (cons (subseq key 4)
               (loop for val/id in (split-sequence:split-sequence #\space values)
                  for (val id) = (split-sequence:split-sequence #\: val/id)
                  for asset = (and wallet
                                   (find id assets :test #'equal :key #'asset-id))
                  for num = (if asset
                                (format-loom-qty-from-asset val asset)
                                val)
                  collect (cons id num))))))

(defun initialize-usage-issuer (location &optional is-passphrase-p)
  "Move the usage token issuer to LOCATION in a new database.
LOCATION is a passphrase if IS-PASSPHRASE-P is true.
Return the location."
  (setf location (passphrase-location location is-passphrase-p))
  (grid-buy *zero* *zero* *zero*)
  (grid-buy *zero* location *zero*)
  (grid-issuer *zero* *zero* location)
  location)

(defun initialize-new-store (admin-passphrase &key
                             (name "Server Admin") assets locations)
  "Initialize a new Loom store, using ADMIN-PASSPHRASE as the passphrase
for the usage token issuer.
NAME is the name of the account. Default: \"Server Admin\".
ASSETS may be a list of ASSET instances to add.
LOCATIONS may be a list of LOCATION instances to add."
  (with-loom-transaction ()
    (let ((loc (initialize-usage-issuer admin-passphrase t)))
      (create-wallet admin-passphrase loc
                     :name name
                     :sponsor-name nil
                     :assets assets
                     :locations locations))))

(defun add-asset (location id name &optional scale precision (usage location))
  "Add a new asset to the wallet at LOCATION, with the given
NAME, ID, SCALE, & PRECISION. SCALE & PRECISION default to NIL.
Charge usage tokens to USAGE, default LOCATION.
Error if NAME or ID already exists with different properties.
Returns ID."
  (check-type location loom-loc)
  (check-type name string)
  (check-type scale (or null integer))
  (check-type precision (or null integer))
  (check-type id loom-loc)
  (with-loom-transaction ()
    (let* ((wallet (get-wallet location))
           (assets (wallet-assets wallet)))
      (awhen (find-asset name assets)
        (unless (and (eql scale (asset-scale it))
                     (eql precision (asset-precision it))
                     (equal id (asset-id it)))
          (error "Asset already exists with different properties: ~s" name))
        (return-from add-asset id))
      (awhen (find-asset-by-id id assets)
        (error "Asset ID already exists with different properties: ~s" id))
      (setf (wallet-assets wallet)
            (nconc assets
                   (list (make-asset
                          :name name :scale scale :precision precision :id id))))
      (setf (get-wallet location nil usage) wallet)
      id)))

(defun remove-asset (location id &optional error-if-absent-p (usage location))
  "Remove the asset with the given ID from the wallet at LOCATION.
Error if ERROR-IF-ABSENT-P is true and their is no such asset.
Credit usage tokens to USAGE, default LOCATION."
  (with-loom-transaction ()
    (let* ((wallet (get-wallet location))
           (asset (find-asset-by-id id wallet)))
      (cond (asset
             (setf (wallet-assets wallet)
                   (delete id (wallet-assets wallet)
                           :test #'equal :key #'asset-id)
                   (get-wallet location nil usage) wallet))
            (error-if-absent-p
             (error "The wallet at ~s has no asset with ID: ~s" location id)))))
  nil)

(defun format-loom-qty (integer scale precision)
  "Format a Loom INTEGER as a decimal string using SCALE and PRECISION."
  (check-type integer (integer 0 *))
  (check-type scale (or null (integer 0 *)))
  (check-type precision (or null (integer 0 *)))
  (if (and (or (null precision) (eql precision 0))
           (or (null scale) (eql scale 0)))
      (format nil "~d" integer)
      (let* ((str (format nil "~v,'0d" (1+ scale) integer))
             (len (length str))
             (left-len (- len scale))
             (cutoff (and precision (max 0 (- len left-len precision))))
             (i (1- len))
             left right)
        (when cutoff
          (loop for j from 0 below cutoff
             for ch = (elt str i)
             do
               (unless (eql ch #\0) (return))
               (decf i)))
        (if (< i left-len)
            (setf left (subseq str 0 left-len)
                  right "")
            (setf left (subseq str 0 left-len)
                  right (subseq str left-len (1+ i))))
        (let ((end-cnt (and precision (max 0 (- precision (length right))))))
          (concatenate 'string left "." right
                       (if end-cnt
                           (make-string end-cnt :initial-element #\0)
                           ""))))))

(defun format-loom-qty-from-asset (qty asset)
  "Turn a loom QTY into a printable string, using scale & precision from ASSET."
  (check-type qty (or string integer))
  (check-type asset asset)
  (let* ((scale (asset-scale asset))
         (precision (asset-precision asset))
         (x (if (integerp qty) qty (parse-integer qty)))
         (negative-p (and (< x 0) (setf x (- -1 x))))
         (res (format-loom-qty x scale precision)))
    (if negative-p
        (concatenate 'string "-" res)
        res)))    

(defun unformat-loom-qty (string scale)
  "Turn a decimal string, as returned by FORMAT-LOOM-QTY back into a Loom integer."
  (check-type string string)
  (check-type scale (or null (integer 0 *)))
  (if (or (null scale) (eql scale 0))
      (parse-integer string)
      (let* ((dotpos (position #\. string))
             (len (length string))
             (right-len (if dotpos (- len (1+ dotpos)) 0)))
        (when dotpos
          (setf string
                (concatenate 'string
                             (subseq string 0 dotpos)
                             (subseq string (1+ dotpos)))))
        (if (> right-len scale)
            ;; Truncate a too long decimal
            (parse-integer (subseq string 0 (- len 1 (- right-len scale))))
            ;; Scale a shorter-than-scale decimal
            (* (parse-integer string) (expt 10 (- scale right-len)))))))

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
