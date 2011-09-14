;;;; -*- mode: lisp -*-

(in-package :cl+ssl)

;;;
;;; Test SSL certificate validation
;;; This file isn't normally loaded.
;;; I used it to figure out how the certificate verification works.
;;;

(defparameter *loom-host* "secure.loom.cc")
(defparameter *loom-port* 443)

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
