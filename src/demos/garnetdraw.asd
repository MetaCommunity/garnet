;; garnetdraw.asd				-*-lisp-*-
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

;; NB: This system definition comprises an initial prototype for
;; developing system definitions onto garnet-demos.

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:asdf)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-sys)

(defsystem #:garnetdraw
  :default-component-class garnet-source-file
  :version "2.0" ;; NB: from garnetdraw.lisp
  :serial t
  ;; NB: specific gadgets that garnetdraw depends on
  ;; cf. garnetdraw.lisp
  ;;
  ;; FIXME integrate w/ sysdef
  #+NIL ("multi-selection-loader" "polyline-creator-loader"
		    "arrow-line-loader"
		    "motif-menubar-loader"
		    "motif-trill-device-loader"
		    "motif-error-gadget-loader"
		    "motif-save-gadget-loader"
		    "standard-edit-loader")
  :depends-on (#:garnet-gadgets #:garnet-ps)
  :components ((:file "garnetdraw-package")
               (:file "garnetdraw")))
