;; garnet-desktop-shared.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared)
  (asdf:operate 'asdf:load-op
		'#:info.metacommunity.cltl.utils))

(in-package #:garnet-systems)


(defsystem #:garnet-desktop-shared
  :defsystem-depends-on (#:info.metacommunity.cltl.utils
			 #:garnet-shared)
  :default-component-class garnet-source-file
  :depends-on (#:info.metacommunity.cltl.utils)
  :serial t
  :version "1.0"
  :components 
  ((:file "desktop-shared")
   ))
