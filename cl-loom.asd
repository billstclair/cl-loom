;;;; -*- mode: lisp -*-

(in-package :cl-user)

;;;
;;; Loom in Lisp.
;;; For now, just a client of Patrick Chkoreff's perl code.
;;;

(asdf:defsystem :cl-loom
  :description "Client for Patrick Chkoreff's Loom.cc digital asset trading system."
  :author "Bill St. Clair <billstclair@rayservers.net>"
  :version "0.5"
  :license "Apache"
  :depends-on (anaphora alexandria drakma split-sequence flexi-streams
                        closer-mop trivial-garbage cl-fad cl-crypto)
  :components
  ((:module source
    :serial t
    :components
    ((:file "package")
     (:file "utility")
     (:file "c-escape")
     (:file "loom-client")
     (:file "loom-objects")
     ))))

#-windows
(let* ((dir "~/.local/share/common-lisp/source/"))
  (asdf:run-shell-command "mkdir -p ~a" dir)
  (unless (or (find-package :cl-autorepo)
              (ignore-errors (ql:quickload "cl-autorepo")))
    (let ((autorepo-asd (merge-pathnames "cl-autorepo/cl-autorepo.asd" dir))
          (url "https://github.com/billstclair/cl-autorepo"))
    (asdf:run-shell-command "cd ~a;git clone ~a" dir url)
    (load autorepo-asd)
    (ql:quickload "cl-autorepo"))))

#-windows
(flet ((addit (name)
         (cl-autorepo:add-system
          name (format nil "git://github.com/billstclair/~a.git" name) :git)))
  (addit "cl-crypto"))

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
