;; garnet-c32.asd -*-lisp-*-
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

(defsystem #:garnet-c32
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-shared
	       #:garnet-gilt
	       #:garnet-opal
	       #:garnet-gadgets)
  ;; Gadgets that this system specifically depends on,
  ;; referencing ./c32-loader.lisp
  #+NIL
  ("labeled-box-loader"
   "x-buttons-loader"
   "arrow-line-loader"
   "text-buttons-loader"
   "scrolling-window-loader"
   "scrolling-input-string-loader"
   "scrolling-menu-loader"
   "error-gadget-loader"
   "scrolling-labeled-box-loader"
   "motif-scrolling-window-loader"
   )
  :components ((:file "package")
	       (:file "c32")
	       (:file "c32formula")
	       (:file "c32ref")
	       (:file "pop-up-generalize")
	       (:file "pop-up-copy-formula")
	       (:file "pop-up-ask-object")
	       (:file "pop-up-functions")
	       (:file "c32dialog")
	       (:file "c32-lapidary")))
