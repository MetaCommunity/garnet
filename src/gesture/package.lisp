;; package.lisp - Garnet C32
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2018 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Porting from garnet-loader.lisp
;;
;;------------------------------------------------------------------------------
;;
;; This file contains source code derived from source code that was
;; originally published in garnet-loader.lisp under the following
;; license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing list.
;;
;; Those top-level forms are denoted, below, as derived from
;; garnet-loader.lisp

(in-package #:cl-user)

(defpackage #:agate
  ;; derived from garnet-loader.lisp
  (:use #:common-lisp #:kr)
  ;; also from ./gesture-loader.lisp
  (:export #:do-go #:do-stop)
  )
