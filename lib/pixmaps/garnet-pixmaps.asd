;; garnet-pixmaps.asd			-*-lisp-*-
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

(defsystem #:garnet-pixmaps

  :class resource-system
  :serial t
  :defsystem-depends-on (#:garnet-desktop-shared
                         #:garnet-shared)

  :description "System for Garnet pixmaps"

  ;; FIXME Document the pixmap compoment class
  ;; FIXME Integrate this w/ Garnet runtime code
  ;;        - NB ASDF pathname translations
  :components
  ((pixmap "arrdown")
   (pixmap "arrleft")
   (pixmap "arrne")
   (pixmap "arrright")
   (pixmap "arrup")
   (pixmap "bee")
   (pixmap "block")
   (pixmap "bomb")
   (pixmap "bull")
   (pixmap "circles")
   (pixmap "city")
   (pixmap "crab45")
   (pixmap "crab")
   (pixmap "doso")
   (pixmap "doss")
   (pixmap "dos")
   (pixmap "editor")
   (pixmap "escherknot")
   (pixmap "eye10")
   (pixmap "eye11")
   (pixmap "eye12")
   (pixmap "eye13")
   (pixmap "eye14")
   (pixmap "eye15")
   (pixmap "eye16")
   (pixmap "eye17")
   (pixmap "eye18")
   (pixmap "eye19")
   (pixmap "eye1")
   (pixmap "eye20")
   (pixmap "eye21")
   (pixmap "eye22")
   (pixmap "eye23")
   (pixmap "eye24")
   (pixmap "eye2")
   (pixmap "eye3")
   (pixmap "eye4")
   (pixmap "eye5")
   (pixmap "eye6")
   (pixmap "eye7")
   (pixmap "eye8")
   (pixmap "eye9")
   (pixmap "eye")
   (pixmap "fils")
   (pixmap "fil")
   (pixmap "floppy")
   (pixmap "garnetlogo")
   (pixmap "jolifond")
   (pixmap "koala")
   (pixmap "lambda")
   (pixmap "martini")
   (pixmap "mickey")
   (pixmap "mouse")
   (Pixmap "NeXT")
   (pixmap "nobozos")
   (pixmap "noseback")
   (pixmap "nosefront")
   (pixmap "nose")
   (pixmap "nosmoking")
   (Pixmap "Plaid")
   (pixmap "porsche")
   (pixmap "stopsign")
   (pixmap "test2")
   (pixmap "test3")
   (pixmap "umbrella")
   (pixmap "wave2")
   (pixmap "wingdogs")
   ))

;; example application:
;; (find-bitmap-pathname "NeXT" :system "garnet-pixmaps")
