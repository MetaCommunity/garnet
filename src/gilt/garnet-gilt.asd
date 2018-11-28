;; garnet-gilt.asd			-*-lisp-*-
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
;;   please contact garnet@cs.cmu.edu to be put on the mailing li

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:asdf)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-sys)

(defsystem #:garnet-gilt
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-pixmaps ;; NB - Needs testing
	       #:garnet-gadgets
	       )
  :defsystem-depends-on (#:garnet-shared)
  ;; NB Gadgets that this system specifically depends on,
  ;; referncimg ./gilt-loader.lisp
  #+NIL
  ("motif-error-gadget-loader"
   "motif-prop-sheet-win-loader"
   "multi-selection-loader"
   "motif-text-buttons-loader"
   "motif-scrolling-labeled-box-loader"
   "motif-scrolling-window-loader"
   "motif-radio-buttons-loader"
   "motif-menubar-loader"
   "motif-menu-loader"
   "motif-check-buttons-loader"
   "motif-slider-loader"
   "standard-edit-loader"
   #+lucid "option-button-loader"
   #+lucid "popup-menu-button-loader"
   )

  ;; FIXME see gilt-loader.lisp
  :components
  (#+NIL ;; FIXME: Map into defsystem
   (:file "gilt-functions-loader")
   #+NIL  ;; FIXME: Map into defsystem
   (:file "filter-functions-loader")
   #+NIL  ;; FIXME: Map into defsystem
   (:file "path-functions-loader")

   (:file "gilt-gadget-utils")
   (:file "motif-gilt-gadgets")
   (:file "gilt-gadgets")
   (:file "gilt")
   (:file "line-imp")
   (:file "motif-line-props")
   (:file "fill-imp")
   (:file "motif-fill-props")
   (:file "align")
   (:file "motif-gilt-save")
   (:file "motif-gilt-read")
   (:file "gilt-font-imp")
   (:file "motif-gilt-font-props")
   (:file "color-imp")
   (:file "motif-color-props")
   (:file "value-control")
   (:file "enable-control")
   (:file "error-check")
   ))
