;;;; -*- mode: lisp -*-

(in-package :cl+ssl)

;;;
;;; Patches to cl+ssl adding certificate verification.
;;;

(export '(ssl-verify-init
          ssl-check-verify-p
          ssl-error-verify-code
          ssl-error-verify-keyword
          ssl-stream-handle
          ssl-stream-x509-certificate
          ssl-get-peer-certificate
          x509-certificate-names
          ))

(cffi:defcfun ("SSL_CTX_set_verify_depth" ssl-ctx-set-verify-depth)
    :void
  (ctx :pointer)
  (depth :int))

(cffi:defcfun ("SSL_get_verify_result" ssl-get-verify-result)
    :long
  (ssl ssl-pointer))

(cffi:defcfun ("SSL_get_peer_certificate" ssl-get-peer-certificate)
    :pointer
  (ssl ssl-pointer))

(cffi:defcfun ("X509_NAME_oneline" x509-name-oneline)
    :pointer
  (x509-name :pointer)
  (buf :pointer)
  (size :int))

(cffi:defcfun ("X509_get_issuer_name" x509-get-issuer-name)
    :pointer                            ; *X509_NAME
  (x509 :pointer))

(cffi:defcfun ("X509_get_subject_name" x509-get-subject-name)
    :pointer                            ; *X509_NAME
  (x509 :pointer))

(defun x509-certificate-names (x509-certificate)
  (unless (cffi:null-pointer-p x509-certificate)
    (cffi:with-foreign-pointer (buf 1024)
      (let ((issuer-name (x509-get-issuer-name x509-certificate))
            (subject-name (x509-get-subject-name x509-certificate)))
        (values
         (unless (cffi:null-pointer-p issuer-name)
           (x509-name-oneline issuer-name buf 1024)
           (cffi:foreign-string-to-lisp buf))
         (unless (cffi:null-pointer-p subject-name)
           (x509-name-oneline subject-name buf 1024)
           (cffi:foreign-string-to-lisp buf)))))))

(defmethod ssl-stream-handle ((stream flexi-streams:flexi-stream))
  (ssl-stream-handle (flexi-streams:flexi-stream-stream stream)))

(defun ssl-stream-x509-certificate (ssl-stream)
  (ssl-get-peer-certificate (ssl-stream-handle ssl-stream)))

(defun ssl-load-global-verify-locations (&rest pathnames)
  (dolist (path pathnames)
    (let ((namestring (namestring (truename path))))
      (cffi:with-foreign-strings ((cafile namestring))
        (unless (eql 1 (ssl-ctx-load-verify-locations
                        *ssl-global-context*
                        cafile
                        (cffi:null-pointer)))
          (error "ssl-ctx-load-verify-locations failed."))))))

(defvar *ssl-check-verify-p* :unspecified)

(defun ssl-check-verify-p ()
  (and *ssl-check-verify-p* (not (eq *ssl-check-verify-p* :unspecified))))

(defun (setf ssl-check-verify-p) (boolean)
  (setf *ssl-check-verify-p* (not (null boolean))))

(defun ssl-verify-init (&key
                        (verify-depth nil)
                        (verify-locations nil))
  (check-type verify-depth (or null integer))
  (ensure-initialized)
  (when verify-depth
    (ssl-ctx-set-verify-depth *ssl-global-context* verify-depth))
  (when verify-locations
    (apply #'ssl-load-global-verify-locations verify-locations)
    ;; This makes (setf (ssl-check-verify) nil) persistent
    (unless (null *ssl-check-verify-p*)
      (setf (ssl-check-verify-p) t))
    t))

(defparameter *ssl-verify-error-alist*
  '((0 :X509_V_OK)
    (2 :X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT)
    (3 :X509_V_ERR_UNABLE_TO_GET_CRL)
    (4 :X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE)
    (5 :X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE)
    (6 :X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY)
    (7 :X509_V_ERR_CERT_SIGNATURE_FAILURE)
    (8 :X509_V_ERR_CRL_SIGNATURE_FAILURE)
    (9 :X509_V_ERR_CERT_NOT_YET_VALID)
    (10 :X509_V_ERR_CERT_HAS_EXPIRED)
    (11 :X509_V_ERR_CRL_NOT_YET_VALID)
    (12 :X509_V_ERR_CRL_HAS_EXPIRED)
    (13 :X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD)
    (14 :X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD)
    (15 :X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD)
    (16 :X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD)
    (17 :X509_V_ERR_OUT_OF_MEM)
    (18 :X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT)
    (19 :X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN)
    (20 :X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY)
    (21 :X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE)
    (22 :X509_V_ERR_CERT_CHAIN_TOO_LONG)
    (23 :X509_V_ERR_CERT_REVOKED)
    (24 :X509_V_ERR_INVALID_CA)
    (25 :X509_V_ERR_PATH_LENGTH_EXCEEDED)
    (26 :X509_V_ERR_INVALID_PURPOSE)
    (27 :X509_V_ERR_CERT_UNTRUSTED)
    (28 :X509_V_ERR_CERT_REJECTED)
    (29 :X509_V_ERR_SUBJECT_ISSUER_MISMATCH)
    (30 :X509_V_ERR_AKID_SKID_MISMATCH)
    (31 :X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH)
    (32 :X509_V_ERR_KEYUSAGE_NO_CERTSIGN)
    (50 :X509_V_ERR_APPLICATION_VERIFICATION)))

(defun ssl-verify-error-keyword (code)
  (cadr (assoc code *ssl-verify-error-alist*)))

(defun ssl-verify-error-code (keyword)
  (caar (member keyword *ssl-verify-error-alist* :key #'cadr)))

(define-condition ssl-error-verify (ssl-error)
  ((error-code :initarg :error-code
               :reader ssl-error-code))
  (:report (lambda (condition stream)
             (let ((code (ssl-error-code condition)))
               (format stream "SSL verify error: ~d~@[ ~a~]"
                       code (ssl-verify-error-keyword code)))
	     (write-queued-errors condition stream))))

(defun ssl-stream-check-verify (ssl-stream)
  (let* ((handle (ssl-stream-handle ssl-stream))
         (err (ssl-get-verify-result handle)))
    (unless (eql err 0)
      (error 'ssl-error-verify :error-code err))))

;; Redefined from cl+ssl/streams.lisp
;; Added the 
(defun make-ssl-client-stream
    (socket &key certificate key password (method 'ssl-v23-method) external-format
                 close-callback (unwrap-stream-p t))
  "Returns an SSL stream for the client socket descriptor SOCKET.
CERTIFICATE is the path to a file containing the PEM-encoded certificate for
 your client. KEY is the path to the PEM-encoded key for the client, which
may be associated with the passphrase PASSWORD."
  (ensure-initialized :method method)
  (let ((stream (make-instance 'ssl-stream
			       :socket socket
			       :close-callback close-callback))
        (handle (ssl-new *ssl-global-context*)))
    (setf socket (install-handle-and-bio stream handle socket unwrap-stream-p))
    (ssl-set-connect-state handle)
    (with-pem-password (password)
      (install-key-and-cert handle key certificate))
    (ensure-ssl-funcall stream handle #'ssl-connect handle)
    (when (ssl-check-verify-p)
      (ssl-stream-check-verify stream))
    (handle-external-format stream external-format)))

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
