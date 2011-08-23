(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun conflicting-symbols-between (base import)
    (unless (packagep base) (setf base (find-package base)))
    (unless (packagep import) (setf import (find-package import)))
    (let (collect)
      (mapcar (lambda (package)
                (do-external-symbols (v package)
                  (multiple-value-bind (symbol ext)
                      (find-symbol (symbol-name v) import)
                    (when (eq ext :external)
                      (push symbol collect)))))
              (cons base (package-use-list base)))
      collect)))

(defpackage loom
  (:use :closer-mop
        :common-lisp
        :trivial-garbage
        :alexandria
        :anaphora)
  (:nicknames lm)
  ;; Shadow conflicting closer-mop symbols before :use
  #.`(:shadowing-import-from :closer-mop
                             ,@(conflicting-symbols-between :cl :closer-mop))
  (:shadowing-import-from :alexandria #:lastcar)
  (:import-from :cl-user #:conflicting-symbols-between)
  (:export #:c-escape
           #:c-unescape

           #:*loom-server*
           #:with-loom-server
           #:with-loom-transaction

           #:loom-server
           #:base-uri-of
           #:setup-of
           #:make-configuration
           #:make-uri-configuration
           #:make-loom-server
           #:make-loom-uri-server

           #:loom-server-setup
           #:make-loom-server-setup
           #:loom-server-setup-host
           #:loom-server-port
           #:loom-server-path
           #:loom-server-use-ssl-p
           #:loom-server-config-dir
           #:loom-server-binary-pathname

           #:initialize-server
           #:loom-client-error
           #:loom-client-error-args
           #:get-loom-error-property

           #:parse-kv
           #:kv-lookup
           #:alist-to-kv-string
           
           #:loom-loc
           #:loom-hash
           
           #:grid-buy
           #:grid-sell
           #:*zero*
           #:grid-issuer
           #:grid-touch
           #:grid-vacant-p
           #:random-vacant-grid-loc
           #:grid-look
           #:grid-move
           #:grid-scan
           #:create-asset
           #:destroy-asset
           
           #:archive-request
           #:archive-buy
           #:archive-sell
           #:archive-touch
           #:archive-vacant-p
           #:random-vacant-archive-loc
           #:archive-look
           #:archive-write
           
           #:random-loc
           #:sha256-function
           #:sha256
           #:grid-hash
           #:archive-hash
           #:format-loom-loc
           #:loom-loc-xor
           #:fold-hash
           #:string-to-hex
           #:parse-hex
           #:print-hex
           #:make-asset-description
           
           #:loom-passphrase
           #:passphrase-location
           #:get-wallet
           #:get-private-wallet
           #:wallet
           #:wallet-assets
           #:wallet-locations
           #:wallet-recording-p
           #:wallet-history
           #:wallet-properties
           #:wallet-get-property
           #:asset
           #:make-asset
           #:asset-name
           #:asset-scale
           #:asset-precision
           #:asset-id
           #:location
           #:make-location
           #:location-name
           #:location-loc
           #:location-disabled-p
           #:location-wallet-p
           #:history
           #:make-history
           #:history-hash
           #:history-time
           #:history-qty
           #:history-type
           #:history-loc
           #:history-memo
           #:find-asset
           #:find-asset-by-id
           #:find-location
           
           #:initialize-usage-issuer
           #:initialize-new-store
           #:create-wallet
           #:move-wallet
           
           #:add-asset
           #:remove-asset
           
           #:format-loom-qty
           #:unformat-loom-qty
           
           ;; loom-objects.lisp
           #:loom-persist
           #:loom-store

           ;; accessors
           #:root-loc-of
           #:usage-loc-of
           
           #:with-loom-store
           #:*root-location-file*
           #:save-root-location-file
           #:make-loom-store
           #:*max-linked-node-length*
           #:*minimum-linked-node-fill*
           #:read-location-dependencies
           #:*loom-in-string*
           #:read-from-location
           #:*loom-out*
           #:write-to-location
           #:define-standard-readers
           #:define-standard-writers
           #:loom-equality-test
           #:determine-class
           #:ensure-loom-class
           #:*force-loom-load*
           #:load-loom-instance
           #:persist-loom-slots
           #:*id*
           #:map-loom-instances-of-class
           #:do-loom-instances-of-class
           #:get-instance-id
           #:class-persisted-slots
           #:remove-loom-object
           #:loom-garbage-collect
           
           #:wallet-loom-store
           ))

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
