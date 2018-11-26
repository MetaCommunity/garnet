;; garnet-gilt.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-gilt
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-pixmaps
	       ;; ...
	       )
  :defsystem-depends-on (#:garnet-shared)
  ;; FIXME see gilt-loader.lisp
  :components ())
