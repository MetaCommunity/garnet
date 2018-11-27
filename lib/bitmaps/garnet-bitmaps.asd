;; garnet-bitmaps.asd				-*-lisp-*-
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2018 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
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

;; NB: Pixmaps data files from original Garnet source code

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:asdf)
  (asdf:find-system '#:garnet-shared)
  (dolist (s '(#:garnet-desktop-shared))
    (asdf:operate 'asdf:load-op s)))

(in-package #:garnet-sys)

(defsystem #:garnet-bitmaps
  :class resource-system
  :serial t
  :defsystem-depends-on (#:garnet-desktop-shared
                         #:garnet-shared)
  :description "System for Garnet bitmaps and cursor resource files"

  :components
  ((bitmap "downarrow")
   (bitmap "indicator")
   (bitmap "pop-up-icon-no-border")
   (bitmap "pop-up-icon")

   (cursor "garbage")

   (cursor "garnet")
   (cursor "hourglass")
   (cursor "lapidary-copy")
   (cursor "lapidary-delete")
   (cursor "lapidary-instance")
   (cursor "lapidary-load")
   (cursor "lapidary-move")
   ;; NB: File "./jagged"
   ;; * No references found, within Garnet source tree
   ;; * File is in a bitmap format
   ;; * No accompanying <name>.mask file
   ;; * Not included in system definition
   (resource-module "garnetdraw"
                    :components
                    ((bitmap "doublelinearrow")
                     (bitmap "linearrow")
                     (bitmap "line")
                     (bitmap "oval")
                     (bitmap "polygon")
                     (bitmap "rectangle")
                     (bitmap "roundrect")))))
