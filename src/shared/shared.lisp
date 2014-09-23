;; shared.lisp - items ported from garnet-loader.lisp

(in-package #:cl-user)

(defparameter Garnet-Version-Number "3.3")
(pushnew :GARNET *features*)
(pushnew :GARNET-V3 *features*)
#+NIL (setf *features* (delete :GARNET-V3.0 *features*))
(pushnew :GARNET-V3.3 *features*)


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
