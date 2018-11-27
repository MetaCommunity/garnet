;; garnet-shared.asd				-*-lisp-*-
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
;; This file contains source code originally published in garnet-loader.lisp
;; under the following license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing list.
;;
;; Those top-level forms are denoted, below, as from garnet-loader.lisp


(in-package #:cl-user)

#|

  This system definition provides a minimum of functionality for
  evaluating Garnet source files with ASDF.

  An effort is made to retain a manner of functionality, analogous to
  the original garnet-loader.lisp.

  The file shared.lisp represents a subset of toplevel froms from the
  original garnet-loader.lisp.

  With this system definition, the package GARNET-SYS is used as
  symbols' home package -- in lieu of the CL-USER package. In effect,
  this may  serve to mitigate a concern with regards to symbol
  definition and symbol reference, in coordinating between the
  evaluation of this system definition -- as a Lisp source file in which
  the package GARNET-SYS is defined -- and the file shared.lisp.

  Concerning any symbols that are defined in garnet-loader.lisp and
  retained in creating this system's shared.lisp, those symbols
  will be exported into the CL-USER package. This is in an effort to
  ensure consistency onto the Garnet documentation.

|#

(eval-when (:compile-toplevel :load-toplevel :execute)

  #-asdf
  (require #:asdf)

  (setf *features*
	(pushnew ':garnet.asdf *features*
		 :test #'eq))


  (let ((shared-symbols
         (list
          ;; - Symbol names from the original garnet-loader.lisp
          (quote #:garnet-version-number)
          (quote #:garnet-garnet-debug)
          (quote #:launch-process-p)
          (quote #:update-locking-p)
          #+allegro (quote #:garnet-readtable)

          ;; - "New" configuration parameters for CL-USER
          (quote #:*compiler-debug-settings*)
          (quote #:*compiler-production-settings*)
          ))
        (cl-user
         (find-package '#:cl-user))
        (garnet-sys
         (or (find-package '#:garnet-sys)
             (defpackage #:garnet-sys
               ;; (:nicknames #:garnet-systems) ;; depr.
               (:use #:asdf #:cl)
               (:export
                ;; internal forms, exported here for possible reuse
                #:with-functions
                )))))
    (declare (type package cl-user garnet-sys))


    (map-into shared-symbols
              #'(lambda (s)
                  (intern (symbol-name s) garnet-sys))
              shared-symbols)

    (export shared-symbols garnet-sys)

    (import shared-symbols cl-user)

    ));; EVAL-WHEN / LET


(in-package #:garnet-sys)

;; -- Mechanisms for COMPUTE-COMPILATION-POLICY

(declaim (type list
               *compiler-debug-settings*
               *compiler-production-settings*))

(defvar *compiler-debug-settings*
  '(optimize (speed 1) (safety 3) (debug 3) (space 1) (compilation-speed 0))
    "Compiler optimization policy for Garnet systems when
GARNET-SYS:GARNET-GARNET-DEBUG is true")

(defvar *compiler-production-settings*
  '(optimize (speed 3) (safety 2) (debug 1) (space 2) (compilation-speed 0))
  "Compiler optimization policy for Garnet systems when
GARNET-SYS:GARNET-GARNET-DEBUG is false")



;; "The :GARNET-DEBUG option allows many different kinds of run-time checking,
;; and also loads some extra test code.  After you have debugged your code
;; and want it to run faster, remove :GARNET-DEBUG from the *features* list
;; and RECOMPILE all of Garnet and your code.  The result will be smaller and
;; somewhat faster.
;;
;; -- from garnet-loader.lisp
;;
(defvar Garnet-Garnet-Debug T)
(if Garnet-Garnet-Debug
    (pushnew :garnet-debug *features*)
    (setf *features* (delete :garnet-debug *features*)))

;; NB: The GARNET-DEBUG feature insofar as defined during EVAL is used
;; in the garnet-kr system, namely in KR::WHEN-DEBUG

;; NB: an EAGER feature is used in the garnet-kr system, e.g in kr-macros.lisp

(defgeneric compute-compilation-policy (op component)
  (:documentation
   "Return a declaration of compiler optimization settings for OP onto COMPONENT

The declaration should be in the format of a list, such as in the following example:

 (optimize (speed 3) (safety 1) (space 1) (debug 1) (compilation-speed 0))"))


;; -- Mechanisms for GARNET-SOURCE-FILE


(defclass garnet-source-file (cl-source-file)
  ()
  (:documentation "Component Class for Lisp Source Files in Garnet"))


(defmethod compute-compilation-policy ((op asdf:compile-op) (c garnet-source-file))
    "If GARNET-SYS:GARNET-GARNET-DEBUG is true, return the value of
GARNET-SYS:*COMPILER-DEBUG-SETTINGS*. Else, return the value of
GARNET-SYS:*COMPILER-PRODUCTION-SETTINGS*"
    (cond
      (garnet-garnet-debug
       (values *compiler-debug-settings*))
      (t
       (values *compiler-production-settings*))))


(defmethod asdf:operate :around ((op compile-op) (c garnet-source-file) &key)
  (with-compilation-unit ()
    (proclaim (compute-compilation-policy op c))
    (call-next-method)))

(defmethod asdf:operate :around ((op load-source-op) (c garnet-source-file) &key)
  (with-compilation-unit ()
    (proclaim (compute-compilation-policy op c))
    (call-next-method)))


#+NIL ;; FIXME QA
(defvar %garnet-sys%
  ;; must be compiled/loaded in this serial order
  ;; pending a closer dependency analysis
  '("utils"
    "kr"
    "gem"
    "opal"
    #-(or allegro CMU) "truetype"
    ;; ^ why is truetype-compiler spec #-(or allegro CMU) in garnet-compiler.lisp?
    ;; note also: the truetype compiler file includes a loop calling
    ;; 'require' [FIXME OLD QA]
    "inter"
    "ps"
    "aggregadgets"
    "gadgets"
    "debug"
    "protected-eval"
    "gesture"
    "demos"
    "gilt"
    "c32"
    "lapidary"))

(defsystem #:garnet-shared ;; FIXME rename => garnet-sys
  :default-component-class garnet-source-file ;; DNW ?!
  :description
  "Shared configuration for Garnet system definitions and Garnet components"
  :components 
  ((:file "shared")
   (:file "sys-utils")
   ))
