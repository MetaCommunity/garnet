;; package.lisp -- garnet gem package definitions

(in-package #:cl-user)

(defpackage #:gem
  (:use #:common-lisp #:kr #:kr-debug))

(defpackage #:opal
  (:use #:common-lisp #:kr))

(defpackage #:interactors
  (:use #:common-lisp #:kr)
  (:nicknames #:inter)
  (:export
   #:*garnet-break-key*
   #:*left-button*
   #:*trans-from-file*))
