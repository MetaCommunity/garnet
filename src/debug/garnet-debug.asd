;; garnet-debug.asd				-*-lisp-*-
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
;; This system contains source code originally published in garnet-loader.lisp
;; under the following license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing list.

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:asdf)
  (asdf:find-system '#:garnet-shared)
  )

(in-package #:garnet-sys)

(defsystem #:garnet-debug
  :version "2.0" ;; NB: from debug-loader.lisp
  :default-component-class garnet-source-file
  ;; Specifically this system depends on the following gadgets:
  ;;
  ;;   multifont-loader
  ;;   text-buttons
  ;;   error-gadget-utils
  ;;
  ;; see also: debug-loader.lisp, debug-compiler.lisp
  ;;
  ;; FIXME: Integrate direct component dependencies w/ system defn
  :serial t
  :version "2.0"
  :defsystem-depends-on (#:garnet-shared)
  :depends-on (#:garnet-gadgets)
  :components ((:file "debug-fns")
	       (:file "objsize")
	       (:file "inspector")
	       (:file "suggest-constants")
	       ))

