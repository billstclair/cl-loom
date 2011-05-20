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

(with-system-name (:loom)

  (define-root-directory :loom  *load-truename*)
  
  (define-module :loom
    (:directory :loom)
    (:files "package"
            "c-escape"
            "loom-client"
	    )))
