;; garnet-ps.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-ps
  :version "1.0"
  :default-component-class garnet-source-file
  :serial t
  :defsystem-depends-on (#:garnet-shared)
  :depends-on (#:garnet-opal)
  ;; NOTES:
  ;; * Garnet-PS defines objects in the #:OPAL package
  ;; * Garnet-PS depends specifically on the 'multifont'
  ;;   component within the #:Garnet-Opal system
  :components ((:file "ps")
	       (:file "ps-multifont")))
