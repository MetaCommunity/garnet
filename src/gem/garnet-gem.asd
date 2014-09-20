;; garnet-gem.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-gem
  :default-component-class garnet-source-file
  :serial t
  :version "1.0"
  :depends-on (#:garnet-shared #:clx #:garnet-kr)
  :perform (load-op :after (o c)
	       (with-safe-frefs ((ensure #:ensure-device-initializer
					 #:gem)
				 (xtli #:x-top-level-initialize
				       #:gem))
		 (funcall ensure :x xtli)))
  :components ((:file "package")
               (:file "gem")
               (:file "define-methods")
               (:file "x")
               ))
