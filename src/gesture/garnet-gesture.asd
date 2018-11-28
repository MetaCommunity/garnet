;; garnet-gesture.asd				-*-lisp-*-
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2018 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Porting from garnet-loader.lisp onto ASDF
;;
;;------------------------------------------------------------------------------
;;
;; This system contains source code derived from source code that was
;; originally published under the following license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing lis

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:asdf)
  (asdf:find-system '#:garnet-shared)
  )

(in-package #:garnet-sys)

(defsystem #:garnet-gesture
  :default-component-class garnet-source-file
  :version "1.0" ;; from gesture-interactor-version-number in ./gesture-loader.lisp
  :serial t
  ;; FIXME QA these DEPS
  :depends-on (#:garnet-shared
	       #:garnet-gilt
	       #:garnet-opal
	       #:garnet-gadgets)
  :components
  ((:file "package") ;; NB The #:AGATE Package - See also: GARNET-GEM system
   (:file "gestureinter")
   (:file "classify")
   (:file "features")
   (:file "fileio")
   (:file "matrix")
   ))
