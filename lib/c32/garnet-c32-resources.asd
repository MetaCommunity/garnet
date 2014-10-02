;; garnet-c32-resources.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:garnet-desktop-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-c32-resources
  :default-component-class garnet-source-file
  :serial t
  :defsystem-depends-on (#:garnet-desktop-shared)
  :components ((bitmap "formula-icon")
               (bitmap "inherited-icon")))
