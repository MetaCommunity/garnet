;; package.lisp -- garnet kr package definitions

(in-package #:cl-user)

(defpackage #:kr-debug
  (:use #:common-lisp))

(defpackage #:kr
  (:use #:common-lisp #:kr-debug))
