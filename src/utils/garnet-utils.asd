;; garnet-utils.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-utils
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-shared)
  :components ((:file "package")
               (:file "general")))
