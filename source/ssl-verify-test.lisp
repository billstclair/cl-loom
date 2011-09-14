;;;; -*- mode: lisp -*-

(in-package :cl+ssl)

;;;
;;; Test SSL certificate validation
;;; This file isn't normally loaded.
;;; I used it to figure out how the certificate verification works.
;;;

(defparameter *loom-host* "secure.loom.cc")
(defparameter *loom-port* 443)

;; from cl+ssl/example.lisp
(defun read-line-crlf (stream &optional eof-error-p)
  (let ((s (make-string-output-stream)))
    (loop
        for empty = t then nil
	for c = (read-char stream eof-error-p nil)
	while (and c (not (eql c #\return)))
	do
	  (unless (eql c #\newline)
	    (write-char c s))
	finally
	  (return
	    (if empty nil (get-output-stream-string s))))))

;; from cl+ssl/example.lisp
(defun test-https-client (host &key (port 443) show-text-p)
  (let* ((deadline (+ (get-internal-real-time)
		      (* 3 internal-time-units-per-second)))
	 (socket (ccl:make-socket :address-family :internet
				  :connect :active
				  :type :stream
				  :remote-host host
				  :remote-port port
;;  				  :local-host (resolve-hostname local-host)
;; 				  :local-port local-port
				  :deadline deadline))
         (https
	  (progn
	    (cl+ssl:make-ssl-client-stream
	     socket
	     :unwrap-stream-p t
	     :external-format '(:iso-8859-1 :eol-style :lf)))))
    (unwind-protect
	(progn
          (let* ((raw-ssl-stream (flexi-streams:flexi-stream-stream https))
                 (ssl (ssl-stream-handle raw-ssl-stream))
                 (cert (ssl-get-peer-certificate ssl))
                 (err (ssl-get-verify-result ssl)))
            (format t "Certificate: ~s, err: ~s~%" cert err)
            (unless (cffi:null-pointer-p cert)
              (multiple-value-bind (issuer subject)
                  (x509-names cert)
                (format t "  issuer: ~a~%  subject: ~a~%" issuer subject)))
            (unless (eql err 0)
              (error "SSL verify error: ~s" err)))
	  (format https "GET / HTTP/1.0~%Host: ~a~%~%" host)
	  (force-output https)
	  (loop :for line = (read-line-crlf https nil)
             for cnt from 0
             :while line :do
             (when show-text-p
               (format t "HTTPS> ~a~%" line))
             finally (return cnt)))
      (close https))))

(defun test-loom-client (&optional show-text-p)
  (test-https-client *loom-host* :port *loom-port* :show-text-p show-text-p))


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
