;;;; -*- mode: lisp -*-

(in-package :module-manager-user)

;;;
;;; Loom in Lisp.
;;; For now, just a client of Patrick Chkoreff's perl code.
;;;

;; Required packages for client
(require :asdf)
(asdf:oos 'asdf:load-op 'anaphora)
(asdf:oos 'asdf:load-op 'alexandria)
(asdf:oos 'asdf:load-op 'drakma)
(asdf:oos 'asdf:load-op 'split-sequence)
(asdf:oos 'asdf:load-op 'flexi-streams)
(asdf:oos 'asdf:load-op 'closer-mop)

(with-system-name (:cl-loom)
  (define-root-directory :cl-loom
      *load-truename*)
  (define-module :cl-loom
    (:directory :cl-loom)
    (:files "package"
            "c-escape"
            "loom-client"
            "loom-objects"
	    )))

(in-package :cl-user)

(defparameter *cl-loom-source-file* *load-truename*)

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
