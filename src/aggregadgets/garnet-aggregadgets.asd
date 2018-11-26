;; garnet-aggregadgets.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)



(defsystem #:garnet-aggregadgets
  :default-component-class garnet-source-file
 :serial t
  :depends-on (#:garnet-shared
               #:garnet-opal
               #:garnet-inter
               ;; FIXME document the following conditional
	       #-(or allegro CMU) #:clx-truetype
	       )
  :components
  ((:file "package")
   (:file "agg-macros")
   (:file "agg-utils")
   (:file "aggregadgets")
   (:file "aggrelists")
   (:file "add-agg")
   (:file "agg-fix-slots")
   (:file "copy-agg")
   (:file "save-agg")
   (:file "string-edit")
   (:file "agg-labels")
   (:file "rectangle-conflict-object")
   (:file "aggregraphs")
   (:file "scalable-aggregraph")
   (:file "scalable-aggregraph-image")
   ))

;; (append-default-directory :up "gadgets")
