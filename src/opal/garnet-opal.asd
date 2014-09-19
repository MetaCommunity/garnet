;; garnet-opal.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-opal
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-shared #:garnet-gem)
  :components
  ((:file "package") ;; defines garnet-gadgets package
   (:file "types")
   (:file "update-constants")
   (:file "defs")
   (:file "macros")
   (:file "new-defs")
   (:file "utils")
   (:file "text-fonts")
   (:file "create-instances")
   (:file "create-instances2")
   (:file "text-functions")
   (:file "text")

   (:file "update-basics")
   (:file "halftones")
   (:file "objects")
   (:file "roundtangles")
   (:file "basics")
   (:file "aggregates")
   (:file "process")
   (:file "clean-up")
   (:file "windows")
   (:file "update")
   (:file "fast-redraw")
   (:file "update-window")
   (:file "multifont")
   (:file "virtual-aggregates")
   (:file "pixmaps")
   (:file "open-and-close")
   #+NIL (:file "x")
   ))
