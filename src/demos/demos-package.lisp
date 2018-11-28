;; demo-package.lisp -- garnetdraw package definitions
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
;; This file contains source code derived from source code that was
;; originally published in garnet-loader.lisp under the following
;; license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing list.
;;
;; Those top-level forms are denoted, below, as derived from
;; garnet-loader.lisp

(in-package #:cl-user)


;; NB see also: garnetdraw-packgage.lisp

;; all of the following are drived from garnet-loader.lisp

(defpackage #:demos-controller
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-3d
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-multiwin
  (:use #:kr #:common-lisp)
  (:export #:do-go #:do-stop))

(defpackage #:demo-multifont
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-animator
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-angle
  (:use #:kr #:common-lisp)
  (:export #:do-go #:do-stop))

(defpackage #:demo-othello
  (:use #:kr #:common-lisp)
  (:nicknames :doth)
  (:export #:do-go #:do-stop #:start-game #:stop-game #:set-score))

(defpackage #:mge
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop
           #:create-piece #:destroy-piece #:destroy-all-pieces
           #:go-initialize #:editor-show-window))

(defpackage #:demo-pixmap
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-arith
  (:use #:kr #:common-lisp)
  (:export #:do-go #:do-stop))

(defpackage #:demo-schema-browser
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop
           #:schema-browser #:schema-browser-win
           #:schema-browser-top-agg))

(defpackage #:demo-array
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-scrollbar
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop
           #:MAC-obj #:MAC-Go #:MAC-Stop
           #:Open-obj #:Open-Go #:Open-Stop
           #:NEXT-obj #:NEXT-Go #:NEXT-Stop
           #:Motif-obj #:Motif-Go #:Motif-Stop))

(defpackage #:demo-clock
  (:use #:kr #:common-lisp)
  (:export #:do-go #:do-stop))

(defpackage #:demo-sequence
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-editor
  (:use #:kr #:common-lisp)
  (:export #:do-go #:do-stop))

(defpackage #:demo-text
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-file-browser
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop #:file-browser #:file-browser-win
           #:file-browser-top-agg))

(defpackage #:demo-truck
  (:use #:kr #:common-lisp)
  (:export #:do-go #:do-stop))

(defpackage #:demo-gadgets
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-twop
  (:use #:kr #:common-lisp)
  (:export #:do-go #:do-stop))

(defpackage #:demo-gesture
  (:use #:kr #:common-lisp)
  (:export #:do-go #:do-stop))

(defpackage #:demo-unistrokes
  (:use #:common-lisp #:kr :inter)
  (:export #:do-go #:do-stop))

(defpackage #:demo-graph
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop
           #:schema-graph #:demo-graph-error-gadget
           #:root-box #:relayout #:demo-graph-win))

(defpackage #:demo-virtual-agg
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-grow
  (:use #:kr #:common-lisp)
  (:export #:do-go #:do-stop))

(defpackage #:demo-xasperate
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-logo
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop #:re-animate))

(defpackage #:demo-manyobjs
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-menu
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:garnet-calculator
  (:use #:common-lisp #:kr)
  (:export #:start-calc #:stop-calc #:do-go #:do-stop))

(defpackage #:demo-mode
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-motif
  (:use #:common-lisp #:kr)
  (:export #:do-go #:do-stop))

(defpackage #:demo-moveline
  (:use #:kr #:common-lisp)
  (:export #:do-go #:do-stop))
