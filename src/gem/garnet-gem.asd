;; garnet-gem.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-gem
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-shared #:clx #:garnet-kr)
  :components ((:file "package")
               (:file "gem")
               (:file "define-methods")
               (:file "x")
               ))
