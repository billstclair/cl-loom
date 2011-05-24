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

(with-system-name (:cl-loom)

  (define-root-directory :cl-loom  *load-truename*)
  
  (define-module :cl-loom
    (:directory :cl-loom)
    (:files "package"
            "c-escape"
            "loom-client"
	    )))
