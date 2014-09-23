;; garnet-debug.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-debug
  :version "2.0"
  :default-component-class garnet-source-file
  ;; Specifically this system depends on the following gadgets:
  ;;
  ;;   multifont-loader
  ;;   text-buttons
  ;;   error-gadget-utils
  ;;
  ;; see also: debug-loader.lisp
  :serial t
  :version "2.0"
  :depends-on (#:garnet-shared #:garnet-gadgets
			       #:garnet-bitmaps
			       #:garnet-pixmaps)
  :components ((:file "debug-fns")
	       (:file "objsize")
	       (:file "inspector")
	       (:file "suggest-constants")
	       ))

