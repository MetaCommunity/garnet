;; garnet-demos.asd				-*-lisp-*-
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
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-sys)

(defsystem #:garnet-demos
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-shared
               #:garnet-opal ;; incl. multifont
               #:garnet-gadgets
               #:garnet-aggregadgets ;; incl. aggregraphs
               #:garnetdraw
               ;; #:garnet-protected-eval ;; not used in demos (??)
               #:garnet-gesture
               #:garnet-ps
               )
  ;; specific gadgets this system depends on:
  ;; * demos-controller
  ;;   - multifont
  ;;   - x-buttons
  ;;   - text-buttons
  ;;   - scrolling-window
  ;;   - mouseline
  ;; * [...]
  :components ((:file "demos-package")
               ;; cf. demos-compiler.lisp
               (:file "demo-3d")
               (:file "demo-angle")
               (:file "demo-animator")
               (:file "demo-arith")
               (:file "demo-array")
               (:file "garnet-calculator")
               (:file "demo-virtual-agg")
               (:file "demo-clock")
               (:file "demo-editor")
               (:file "demo-file-browser")
               (:file "demo-gadgets")
               (:file "demo-gesture")
               (:file "demo-graph")
               (:file "demo-grow")
               (:file "demo-logo")
               (:file "demo-manyobjs")
               (:file "demo-menu")
               (:file "demo-mode")
               (:file "demo-motif")
               (:file "demo-moveline")
               (:file "demo-multifont")
               (:file "demo-multiwin")
               (:file "demo-pixmap")
               (:file "demo-schema-browser")
               (:file "demo-scrollbar")
               (:file "demo-sequence")
               (:file "demo-text")
               (:file "demo-truck")
               (:file "demo-twop")
               (:file "mge")
               (:file "demo-othello")
               (:file "demo-xasperate")
               (:file "demo-unistrokes")
               ;; (:file "garnetdraw") ;; available via garnetdraw.asd
               (:file "demos-controller")
               (:file "tour")
               ))
