;; package.lisp -- package definitions for gem, opal, interactors systems
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

;; NB: Along with the GEM package, The OPAL and INTER packages are also
;; defined here, in an effort to alleviate concerns with regards to
;; forward references when compiling or otherwise evaluating source code
;; in the Garnet system [spchamp]

(in-package #:cl-user)

(defpackage #:gem
  ;; derived from garnet-loader.lisp
  (:use #:common-lisp
	#:kr #:kr-debug)
  (:export #:active-devices
	   #:init-device))

(defpackage #:opal
  ;; derived from garnet-loader.lisp
  (:use #:common-lisp
	#:kr))

(defpackage #:interactors
  ;; derived from garnet-loader.lisp
  (:use #:common-lisp #:kr)
  (:nicknames #:inter)
  (:export
   #:*garnet-break-key*
   #:*left-button*
   #:*trans-from-file*))
