

;;; Defines the setup to use for the Common Lisp Loom Client
;;; This file will be parsed by read 

(loom-configuration
 (hostname "127.0.0.1")
 (port 9090)
 (path "/")
 (use-ssl nil) ;; t or nil
 (local t) ;; t or nil
 (base-dir #P"/home/lisp/ec/lisp/Loom/")
 (config-dir #P"data/conf")
 (binary-path #P"code/loom")
 )
 
                  
                