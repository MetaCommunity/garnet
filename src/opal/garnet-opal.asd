;; garnet-opal.asd				-*-lisp-*-
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
;; This system contains source code derived from source code that was
;; originally published under the following license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing list.


(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-sys)

(defsystem #:garnet-opal
  :version "1.3" ;; from opal-loader.lisp
  :serial t
  :default-component-class garnet-source-file
  :defsystem-depends-on (#:garnet-shared)
  :depends-on (#:garnet-utils #:garnet-gem #:garnet-bitmaps)
  :perform (load-op :after (o c)
                    ;; FIXME: define an INIT-OP and move this to there
                    (with-functions ((init #:init-device #:gem))
                      ;; FIXME: if the same device is already initialized,
                      ;; do not reinitialize
                      ;;
                      ;; NB: see also, the garnet-gem system definition
                      ;; such that defines the respective device
                      ;; initializer for Gem, within a :PERFORM method
                      ;;
                      ;; NB: referenced onto ./opal-loader.lisp
                      (funcall init
                               #-(and :apple (not :clx)) :X
                               #+(and :apple (not :clx)) :MAC
                               nil)))
  :components
  ((:file "package")
   ;; ^ NB defines only the garnet-gadgets package. See source file
   (:file "types")
   (:file "update-constants")
   (:file "defs")
   (:file "macros")
   (:file "new-defs")
   (:file "utils")
   (:file "text-fonts")
   (:file "create-instances")
   (:file "create-instances2")
   (:file "text-functions")
   (:file "text")

   (:file "update-basics")
   (:file "halftones")
   (:file "objects")
   (:file "roundtangles")
   (:file "basics")
   (:file "aggregates")
   (:file "process")
   (:file "clean-up")
   (:file "windows")
   (:file "update")
   (:file "fast-redraw")
   (:file "update-window")
   (:file "multifont")
   (:file "virtual-aggregates")
   (:file "pixmaps")
   (:file "open-and-close")
   ))
