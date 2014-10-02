;; garnetdraw.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnetdraw
  :default-component-class garnet-source-file
  :version "2.0"
  :serial t
  ;; specific gadgets that garnetdraw depends on
  ;; (see garnetdraw.lisp)
  #+NIL ("multi-selection-loader" "polyline-creator-loader"
		    "arrow-line-loader"
		    "motif-menubar-loader"
		    "motif-trill-device-loader"
		    "motif-error-gadget-loader"
		    "motif-save-gadget-loader"
		    "standard-edit-loader")
  :depends-on (#:garnet-gadgets #:garnet-ps)
  :defsystem-depends-on (#:garnet-shared)
  :components ((:file "garnetdraw")))
