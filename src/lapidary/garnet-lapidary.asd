;; garnet-lapidary.asd				-*-lisp-*-
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

(defsystem #:garnet-lapidary
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-c32
	       #:garnet-debug
	       #:garnet-gilt
	       #:garnet-gadgets
               #:garnet-aggregadgets)
  ;; FIXME Double-check for forward references
  :components
  ((:file "package")
   (:file "lapidary-functions")
   (:file "parameters")
   (:file "defs")
   (:file "macros")
   (:file "lapidary")
   (:file "dialog-parts2")
   (:file "event-card")
   (:file "card")
   (:file "card1")
   (:file "start-where")
   (:file "prompt")
   (:file "lapidary-objects")
   (:file "feedback-objs")
   (:file "support-misc")
   (:file "support-selection1")
   (:file "support-selection2")
   (:file "selection")
   (:file "create-object")
   (:file "delete-object")
   (:file "delete-window")
   (:file "move-grow")
   (:file "aggregates")
   (:file "aggparam")
   (:file "create-parameters")
   (:file "properties")
   (:file "line-imp")
   (:file "line-props")
   (:file "fill-imp")
   (:file "fill-props")
   (:file "color-imp")
   (:file "color-props")
   (:file "shapes")
   (:file "lap-draw")
   (:file "support-menu-editor")
   (:file "new-editor")
   (:file "text")
   (:file "text-properties")
   (:file "gadgetcopy")
   (:file "save-link-parameters")
   (:file "lapidary-save")
   (:file "lapidary-read")
   (:file "support-save-restore")
   (:file "save-restore")
   (:file "add-gadget")
   (:file "choice-inter")
   (:file "text-inter")
   (:file "move-grow-box")
   (:file "support-move-grow-inter")
   (:file "move-grow-inter")
   (:file "angle-inter")
   (:file "two-point-inter")
   (:file "support-inter")
   (:file "by-demo")
   (:file "interactors")
   (:file "interactors-menu")
   (:module "constraint-gadget"
	    ;; see ./constraint-gadget-loader.lisp
	    :pathname nil
	    :serial t
	    :default-component-class garnet-source-file
	    :components
	    ((:file "cg-defs")
	     (:file "support-constraints")
	     (:file "custom")
	     (:file "attach-constraints")
	     (:file "support-box-constraints")
	     (:file "box-parts")
	     (:file "box")
	     (:file "line-constraint-defs")
	     (:file "line-constraint-objs")
	     (:file "line-constraint")
	     (:file "set-feedback")))
   ))
