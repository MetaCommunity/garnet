;; shared.lisp - items ported from garnet-loader.lisp

(in-package #:cl-user)

(defparameter Garnet-Version-Number "3.3")
(pushnew :GARNET *features*)
(pushnew :GARNET-V3 *features*)
#+NIL (setf *features* (delete :GARNET-V3.0 *features*))
(pushnew :GARNET-V3.3 *features*)


;; "The :GARNET-PROCESSES keyword goes on the *features* list if this version
;; of lisp supports multiple processes.  Then things like the animation
;; interactor can use the #+garnet-processes switch, instead of referring
;; explicitly to different versions of lisp."
;; -- garnet-loader.lisp
(pushnew :GARNET-PROCESSES *features*)

;; "launch-process-p controls whether Garnet will launch
;;  a separate process to detect keyboard and mouse events."
;; -- garnet-loader.lisp
(defvar launch-process-p T)

;; "update-locking-p controls whether process locks will be activated
;;  around the update method (this keeps two processes from calling update
;;  at the same time).
;; -- garnet-loader.lisp
(defvar update-locking-p T
  "If T, uses process locks to keep Update in a process from interrupting
   itself in a different process.")


;; "The :GARNET-DEBUG option allows many different kinds of run-time checking,
;; and also loads some extra test code.  After you have debugged your code
;; and want it to run faster, remove :GARNET-DEBUG from the *features* list
;; and RECOMPILE all of Garnet and your code.  The result will be smaller and
;; somewhat faster.
;; "To remove :GARNET-DEBUG from the *features* list, either defvar
;; Garnet-Garnet-Debug to NIL before you load the garnet-loader, or simply
;; comment out the next few lines."
;; -- garnet-loader.lisp
(defvar Garnet-Garnet-Debug T)
(if Garnet-Garnet-Debug
    (pushnew :garnet-debug *features*)
    (setf *features* (delete :garnet-debug *features*)))


#+allegro
(defvar Garnet-Readtable *readtable*
  "This variable is used by Allegro to restore the old value of the *readtable*
when a saved image is restarted (see opal:make-image in opal/utils.lisp).")
