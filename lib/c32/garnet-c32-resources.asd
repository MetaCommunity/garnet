;; garnet-c32-resources.asd			-*-lisp-*-
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
  (asdf:operate 'asdf:load-op '#:garnet-desktop-shared)
  (dolist (s '(#:garnet-desktop-shared))
    (asdf:operate 'asdf:load-op s)))

(in-package #:garnet-sys)

(defsystem #:garnet-c32-resources
  :default-component-class garnet-source-file
  :serial t
  :defsystem-depends-on (#:garnet-desktop-shared)
  :components ((bitmap "formula-icon")
               (bitmap "inherited-icon")))
