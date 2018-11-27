; garnet-kr.asd					-*-lisp-*-
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
;;   please contact garnet@cs.cmu.edu to be put on the mailing list

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:asdf)
  (asdf:find-system '#:garnet-shared)
  )

(in-package #:garnet-sys)

;; FIXME: Parameterize the #+EAGER feature used in kr-macros.lisp
;; such that that feature may be specified via ASDF, when garnet-kr is
;; evaluated for compile or evaulted for loading from source. Ideally,
;; each component using the #+EAGER feature would be identified in the
;; system definition as such.
;;
;; Additionally, it may be helpful towards development if varaibles
;; whose definitions may have a side effect in the operations of the KR
;; system may also be denoted as such, in the system definiton. [spchamp]

(defsystem #:garnet-kr
  :version "2.3.4" ;; from kr-loader.lisp and kr-macros.lisp
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-shared #:garnet-utils)
  :components ((:file "package")
               (:file "kr-macros"
                      ;; :optional-features (:eager)
                      )
               ;; ^ NB Provides numerous system parameters (defvar)
               ;; ^ FIXME Consider porting onto bordeaux-threads,
               ;;   cf. sb-thread annotations in the file
               (:file "kr-doc")
               (:file "kr"
                      ;; :optional-features (:eager)
                      )
               (:file "constraints"
                      ;; :optional-features (:eager)
                      )
               ;; ^ NB: KR::*SETUP-DEPENDENCIES*
               ))
