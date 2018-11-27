;; garnet-c32.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-c32
  :default-component-class garnet-source-file
  :serial t
  :defsystem-depends-on (#:garnet-shared 
			 #:garnet-gilt 
			 #:garnet-opal
			 #:garnet-gadgets)
  ;; Gadgets that this sytsem specifically depends on
  ;; referencing ./c32-loader.lisp
  #+NIL 
  ("labeled-box-loader"
   "x-buttons-loader"
   "arrow-line-loader"
   "text-buttons-loader"
   "scrolling-window-loader"
   "scrolling-input-string-loader"
   "scrolling-menu-loader" 		; for pop-up-functions
   "error-gadget-loader"	  		; for C32error
   "scrolling-labeled-box-loader"	; for package name
   "motif-scrolling-window-loader"
   )
  :components ((:file "package")
	       (:file "c32")
	       (:file "c32formula")
	       (:file "c32ref")
	       (:file "pop-up-generalize")
	       (:file "pop-up-copy-formula")
	       (:file "pop-up-ask-object")
	       (:file "pop-up-functions")
	       (:file "c32dialog")
	       (:file "c32-lapidary")))
