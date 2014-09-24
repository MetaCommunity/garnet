;; package.lisp -- garnet opal package definitions

(in-package #:cl-user)

;; NOTE: Due to system dependencies, the Opal package is defined
;; originally in ../gem/package.lisp
;; similary in ../../garnet-loader.lisp

(defpackage #:garnet-gadgets
  (:use #:common-lisp #:kr)
  (:nicknames #:gg))
