;; garnet-desktop-shared.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared)
  )

(in-package #:garnet-systems)


;; FIXME Document this system definition
;; See also garnet-shared.asd

(defsystem #:garnet-desktop-shared
  :defsystem-depends-on (#:garnet-shared)
  :default-component-class garnet-source-file
  :depends-on (;; FIXME the following sysdef has been renamed
               #:info.metacommunity.cltl.utils
               )
  :serial t
  :version "1.0"
  :components
  ((:file "desktop-shared")
   ))
