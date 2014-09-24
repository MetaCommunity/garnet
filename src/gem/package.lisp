;; package.lisp -- garnet gem package definitions

(in-package #:cl-user)

(defpackage #:gem
  (:use #:common-lisp
	#:kr #:kr-debug)
  (:export #:active-devices
	   #:init-device))

(defpackage #:opal
  (:use #:common-lisp
	#:kr
	#+Garnet.ASDF
	#:info.metacommunity.cltl.utils))

(defpackage #:interactors
  (:use #:common-lisp #:kr)
  (:nicknames #:inter)
  (:export
   #:*garnet-break-key*
   #:*left-button*
   #:*trans-from-file*))
