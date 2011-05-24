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
  :depends-on (anaphora alexandria drakma split-sequence flexi-streams)
  :components
  ((:module source
    :serial t
    :components
    ((:file "package")
     (:file "c-escape")
     (:file "loom-client")
     ))))
