;; garnet-inter.asd			-*-lisp-*-
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
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-sys)

(defsystem #:garnet-inter
  :version "1.0" ;; from inter-loader.lisp
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-shared
               #:garnet-utils
               #:garnet-opal)
  :components
  (;; NB: This system uses the Garnet GEM package

    (:file "garnet-keytrans")
    (:file "define-mouse-keys")

    #-(and apple (not clx)) (:file "x-define-keys")
    #-(and apple (not clx)) (:file "x-inter")

    #+(and apple (not clx)) (:file "mac-define-keys")
    #+(and apple (not clx)) (:file "mac-inter")

    (:file "interactors")
    (:file "accelerators")
    (:file "animation-process")
    (:file "i-windows")
    (:file "menuinter")
    (:file "movegrowinter")
    (:file "buttoninter")
    (:file "twopointinter")
    (:file "textkeyhandling")
    (:file "lispkeyhandling")
    (:file "textinter")
    (:file "multifont-textinter")
    (:file "focus-multifont-textinter")
    (:file "selection-interactor")
    (:file "angleinter")
    (:file "animatorinter")
    ))
