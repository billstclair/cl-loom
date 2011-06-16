(in-package :cl-user)

(defpackage loom
  (:use :closer-mop
        :common-lisp
        :anaphora)
  (:shadowing-import-from closer-mop
                          standard-method
                          standard-generic-function
                          defmethod
                          defgeneric
                          standard-class)
  (:export #:c-escape
           #:c-unescape

           #:*loom-server*
           #:with-loom-server
           #:with-loom-transaction

           #:loom-server
           #:base-uri-of
           #:setup-of
           #:make-loom-server

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
           #:sha256
           #:grid-hash
           #:archive-hash
           #:format-loom-loc
           #:loom-loc-xor
           #:fold-hash
           #:string-to-hex
           #:make-asset-description
           
           #:loom-passphrase
           #:passphrase-location
           #:get-wallet
           #:wallet
           #:wallet-assets
           #:wallet-locations
           #:wallet-recording-p
           #:wallet-history
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
           #:wallet-loom-store
           #:make-loom-store
           #:with-loom-store
           #:loom-persist-standard-object
           #:loom-instance-omitted-slots
           #:map-loom-instances-of-class
           #:do-loom-instances-of-class
           #:loom-instances-of-class
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
