
(in-package #:cl-user)

(defpackage #:lapidary
  (:use #:common-lisp #:kr
        ;; FIXMD rename
	#+Garnet.ASDF #:info.metacommunity.cltl.utils))
