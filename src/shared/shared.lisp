;; shared.lisp - items ported from garnet-loader.lisp

(in-package #:cl-user)

(defparameter Garnet-Version-Number "3.3")
(pushnew :GARNET *features*)
(pushnew :GARNET-V3 *features*)
#+NIL (setf *features* (delete :GARNET-V3.0 *features*))
(pushnew :GARNET-V3.3 *features*)


(defvar *garnet-compile-debug-mode* t
  "Setting this variable to T sets the policy for the entire system
to make it more debuggable.")


(defvar *garnet-compile-debug-settings*
  '(optimize (speed 2) (safety 3) (debug 3) (space 3) (compilation-speed 0))
  "Use these settings for globally debugging the system or for debugging
a specific module. They emphasize debuggability at the cost of some speed.

With SBCL:

- These settings are type-safe. 

- They prevent functions declared inline from being expanded inline. 
  Note that as part of this version I have tried to make most 
  non-syntactic macros into inline functions.

- They allow all possible debugging features.")


(defvar *garnet-compile-production-settings*
  '(optimize (speed 3) (safety 1) (space 0) (debug 2) (compilation-speed 0))
  "Production compiler policy settings. Emphasize speed, de-emphasize debugging.")

  
(defvar *default-garnet-proclaim*
  (if *garnet-compile-debug-mode*
      *garnet-compile-debug-settings*
      *garnet-compile-production-settings*)
  "Set compiler optimization settings.

1. If you want everything debugged, set *garnet-compile-debug-mode* to t.

2. If you want to debug specific modules, set *garnet-compile-debug-mode*
   to nil. Then set the variable in the modules you want debugged to enable
   debugging that module.

3. Otherwise (for 'production' builds) just set *garnet-compile-debug-mode* 
   to nil and leave everything else alone.")


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
