; garnet-kr.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-kr
  :version "2.3.4"
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-shared #:garnet-utils)
  :components ((:file "package")
               (:file "kr-macros")
               (:file "kr-doc")
               (:file "kr")
               (:file "constraints")
               ))
