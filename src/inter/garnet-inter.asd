;; garnet-inter.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-inter
  :version "1.0"
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-shared #:garnet-opal)
  :components
  (#+NIL
   (:file "package") ;; defined in gem package.lisp

   ;; key translation files
    (:file "garnet-keytrans") ;; ???
    (:file "define-mouse-keys")
    (:file "x-define-keys")
    (:file "x-inter")

    ;; interactor files
    (:file "interactors")
    (:file "accelerators")
    (:file "animation-process")
    (:file "i-windows")
    (:file "menuinter")
    (:file "movegrowinter")
    (:file "buttoninter")
    (:file "twopointinter")
    (:file "textkeyhandling")
    (:file "lispkeyhandling")
    (:file "textinter")
    (:file "multifont-textinter")
    (:file "focus-multifont-textinter")
    (:file "selection-interactor")
    (:file "angleinter")
    (:file "animatorinter")
    ))
