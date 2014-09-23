;; garnetdraw.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnetdraw
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-gadgets)
  :defsystem-depends-on (#:garnet-gadgets)
  :components ((:file "garnetdraw")))
