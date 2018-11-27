;; shared.lisp - Top-level forms ported from garnet-loader.lisp
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
;; This file contains source code originally published in garnet-loader.lisp
;; under the following license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing list.
;;
;; Those top-level forms are denoted, below, as from garnet-loader.lisp

;; see also:
;; - garnet:garnet-loader.lisp
;; - garnet:garnet-config.lisp

(in-package #:garnet-sys)

(defparameter Garnet-Version-Number "3.1rc")
(pushnew :GARNET *features*)
(pushnew :GARNET-V3 *features*) ;; new [spchamp]
(pushnew :GARNET-V3.0 *features*)

;; FIXME - Garnet Versioning, in this system
;;
;;  - The file garnet:garnet-loader.lisp defines the version as 3.1rc
;;    and adds GARNET-v3.0 to *FEATURES*. That behavior is retained, here.
;;
;;  - There was a branch in the original garnetlisp source code
;;    repository for Garnet 3.3. After a short review of changesets in
;;    the Garnet 3.3 branch, it may seem that this branch has not
;;    been merged with the garnetlisp trunk.
;;
;; NB - Garnet 2020 / Thinkum-contrib
;;
;; - see e.g garnet:multi-garnet;


;; ''The :GARNET-PROCESSES keyword goes on the *features* list if this version
;;  of lisp supports multiple processes.  Then things like the animation
;;  interactor can use the #+garnet-processes switch, instead of referring
;;  explicitly to different versions of lisp.''
;;
;; -- from garnet-loader.lisp
#+(or allegro lucid lispworks (and cmu mp) ccl-3 sbcl)
(pushnew :GARNET-PROCESSES *features*)

;; ''The :GARNET-BINS option controls whether Garnet uses its own constructed
;;   hash tables called "bins" or uses the system's hash tables at the kernel
;;   of the KR system.  Push :GARNET-BINS onto the *features* list for lisp
;;   implementations that compile to machine code and have slow hash tables.
;;   Don't push it for implementations like CLISP which have fast hash tables.''
;;
;; -- from garnet-loader.lisp
#-CLISP
(pushnew :GARNET-BINS *features*)

;; ''This variable is used by Allegro to restore the old value of the *readtable*
;;   when a saved image is restarted (see opal:make-image in opal/utils.lisp).''
;;
;; -- from garnet-loader.lisp
#+allegro ;; new: only setting this in #+allegro [spchamp]
(defvar Garnet-Readtable *readtable*)

;; ''launch-process-p controls whether Garnet will launch
;;   a separate process to detect keyboard and mouse events.''
;;
;; -- from garnet-loader.lisp
(defvar launch-process-p T)

;; ''update-locking-p controls whether process locks will be activated
;;   around the update method (this keeps two processes from calling update
;;   at the same time).''
;;
;; -- from garnet-loader.lisp
(defvar update-locking-p T
  "If T, uses process locks to keep Update in a process from interrupting
   itself in a different process.")


#+allegro
(defvar Garnet-Readtable *readtable*
  "This variable is used by Allegro to restore the old value of the *readtable*
when a saved image is restarted (see opal:make-image in opal/utils.lisp).")
;; from garnet-loader.lisp
