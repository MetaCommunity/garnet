;;
;; asdf-systems-gen.lisp - generate ASDF system definitions
;;                         for Garnet systems

;; This file contains source code originally published in garnet-loader.lisp
;; under the following license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing list.

;; License: LLGPL 2.1 - see file doc/LLGPL-preamble.txt

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:garnet-systems
    (:use #:asdf #:cl)
    (:shadowing-import-from #:asdf/parse-defsystem
                            #:register-system-definition
                            )
    (:shadowing-import-from #:asdf/system
                            #:system
                            )
    (:shadowing-import-from #:asdf/lisp-action
                            #:load-op
                            #:compile-op)
    (:shadowing-import-from #:asdf/output-translations
                            #:apply-output-translations)
    ))

(in-package  #:garnet-systems)

(defvar %garnet-systems%
  ;; must be compiled/loaded in this serial order
  ;; pending a closer dependency analysis
  '("utils"
    "kr"
    "gem"
    "opal"
    "truetype"
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

#+NIL
(defvar +garnet-compiler-files+
  (mapcar (lambda (sys)
            (format nil "~a-src:~:*~a-compiler" sys))
          %garnet-systems%))



;; --

(in-package #:cl-user)

;; Garnet Pathname Variables

(defvar your-garnet-pathname
  (namestring (make-pathname :directory
                             (pathname-directory *load-truename*))))

(defvar garnet-src-pathname
  (merge-pathnames
   (make-pathname :directory '(:relative "src")
                  :defaults your-garnet-pathname )
   your-garnet-pathname))


(defvar garnet-binary-pathname
  (merge-pathnames
   (make-pathname :directory '(:relative "src")
                  :defaults your-garnet-pathname)
   (asdf:apply-output-translations your-garnet-pathname)))


(defun garnet-probe (pathname &key (prefer-source nil)
                                (source-type "lisp")
                                (errorp t))
  (labels ((foo ()
             (make-pathname
              :name (pathname-name pathname)
              :type (if prefer-source
                        source-type
                        #.(pathname-type  (compile-file-pathname "foo")))
              :defaults
              (translate-logical-pathname pathname)))
           (compute-dest (path prefer-source)
             (cond
               (prefer-source
                (merge-pathnames (make-pathname :type source-type
                                                :defaults path)
                                 garnet-src-pathname))
               (t
                (merge-pathnames (make-pathname
                                  :type #.(pathname-type  (compile-file-pathname "foo"))
                                  :defaults path)
                                 garnet-binary-pathname)))))
      (or (probe-file pathname)
          (let ((foo (foo))
                (pref-dest (compute-dest pathname prefer-source)))
            (or (probe-file foo)
                (probe-file pref-dest)
                (let ((alt-dest (compute-dest pathname (not prefer-source))))
                  (or (probe-file alt-dest)
                      (when errorp
                        (error "Cannot locate file ~s (also tried paths ~s ~s and ~s)"
                               pathname foo pref-dest alt-dest)))))))))



;; (garnet-probe "kr-src:kr-macros" :prefer-source t)
;; (garnet-probe "kr-src:kr-macros")

;; (translate-logical-pathname "kr-src:kr-macros")
;; (translate-logical-pathname "kr-src:kr-macros.lisp")

;; (garnet-probe "kr:kr-macros"  :prefer-source t)
;; (garnet-probe "kr:kr-macros")

;; (garnet-probe "gadgets:GAD-scroll-parts")
;; ^ FIXME. failing only due to pathname case



;; (translate-logical-pathname  "gadgets:GAD-scroll-parts.lisp")
;; ^ NOTE that that "reconverts" the pathname case onto the source
;;   code repository pathname, thus making the pathname seem
;;   "unavailable" though the file exists by a different name case
;;   (Linux) (CLtL2)

;; (garnet-probe "gadgets:GAD-scroll-parts" :prefer-source t)
;; ^ FIXME, "MAKE THAT WORK"

#+NIL
(probe-file (merge-pathnames
             "GAD-scroll-parts"
             (merge-pathnames
              (make-pathname :directory
                             '(:relative "gadgets")
                             :type "lisp"
                             :defaults garnet-src-pathname)
              garnet-src-pathname)))


;; NOTE: A closer conversion to ASDF system definitions
;; may seve to work around the hairy pathname issues

;; Ports of Garnet System Functions

(defun garnet-load (pathname &key (prefer-source nil))
  ;; frob, nothing with regards to garnet-load-alist
  (let ((file (garnet-probe pathname
                            :prefer-source prefer-source)))
    (load file)))

(defun garnet-compile (pathname)
  (let* ((src-file (garnet-probe pathname :prefer-source t))
         (bin-file
          (merge-pathnames
           (make-pathname :type #.(pathname-type
                                   (compile-file-pathname "foo")))
           (asdf:apply-output-translations pathname))))
    (ensure-directories-exist bin-file)
    (compile-file src-file :output-file bin-file)))


(defun garnet-mkdir-if-needed (path)
  (ensure-directories-exist path))

(defun garnet-copy-files (src-dir bin-dir files)
  (let (result)
    (dolist (f files result)
      (let ((src (merge-pathnames f src-dir))
            (dst (merge-pathnames f bin-dir)))
        (uiop/stream:copy-file src dst)
        (setf result (cons dst result))))))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf sb-ext:*muffled-warnings* 'sb-kernel::style-warning))

#+NIL
(when *default-garnet-proclaim*
  (proclaim *default-garnet-proclaim*))

;; -- features and variables directly from garnet-loader.lisp

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(your-garnet-pathname
            garnet-src-pathname
            garnet-binary-pathname
            garnet-load garnet-compile
            garnet-probe ;; new feature in the garnet API
            )))

;; System-specific ports of Garnet <forms>

;; CL-USER::LAUNCH-PROCESS-P : needed by inter/i-windows.lisp

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



;; --------

(in-package #:garnet-systems)

(dolist (sys %garnet-systems%)
  ;; define logical pathnames
  ;;   for <foo>-src: and <foo>: LPN hosts
  ;;
  ;; FIXME: also define variables
  ;;   garnet-<FOO>-pathname (bin)
  ;;   garnet-<FOO>-src (src)
  ;;
  (let* ((src-host-name (format nil "~a-src" sys))
         (src-host-pathname
          (merge-pathnames
           (make-pathname :directory (list :relative sys))
           cl-user:garnet-src-pathname))
         (bin-host-name sys)
         (bin-host-pathname
          (merge-pathnames
           (make-pathname :directory (list :relative sys))
           cl-user:garnet-binary-pathname))
         (lpn-translations
          (list
           (list "**;*.lisp.*"
                 (merge-pathnames (make-pathname :directory
                                                 '(:relative :wild-inferiors)
                                                 :name :wild
                                                 :version :newest
                                                 :type "lisp"
                                                 :defaults src-host-pathname
                                                 )
                                  src-host-pathname))
           (list #.(format nil "**;*.~a.*" (pathname-type
                                            (compile-file-pathname "foo")))
                 (merge-pathnames (make-pathname :directory
                                                 '(:relative :wild-inferiors)
                                                 :name :wild
                                                 :version :newest
                                                 :type
                                                 #.(pathname-type
                                                    (compile-file-pathname "foo"))
                                                 :defaults bin-host-pathname)
                                  bin-host-pathname)))))

    (setf (logical-pathname-translations src-host-name)
          (append lpn-translations
                  (list
                   (list "**;"
                         (merge-pathnames (make-pathname :directory
                                                         '(:relative :wild-inferiors)
                                                         :version :wild
                                                         :defaults src-host-pathname)
                                          src-host-pathname))
                   (list "**;*"
                         (merge-pathnames (make-pathname :directory
                                                         '(:relative :wild-inferiors)
                                                         :name :wild
                                                         :type "lisp"
                                                         :version :wild
                                                         )
                                          src-host-pathname))
                   (list "**;*.*.*"
                              (merge-pathnames (make-pathname :directory
                                                              '(:relative :wild-inferiors)
                                                              :name :wild
                                                              :type :wild
                                                              :version :wild
                                                              )
                                               src-host-pathname)))))

    (setf (logical-pathname-translations bin-host-name)
          (append lpn-translations
                  (list
                   (list "**;"
                         (merge-pathnames (make-pathname :directory
                                                         '(:relative :wild-inferiors)
                                                         :version :wild
                                                         :defaults bin-host-pathname)
                                          bin-host-pathname))
                   (list "**;*"
                         (merge-pathnames (make-pathname
                                           :directory
                                           '(:relative :wild-inferiors)
                                           :name :wild
                                           :type
                                           #.(pathname-type (compile-file-pathname "FOO"))
                                           :version :wild)
                                          bin-host-pathname))
                   (list "**;*.*.*"
                         (merge-pathnames (make-pathname :directory
                                                         '(:relative :wild-inferiors)
                                                         :name :wild
                                                         :type :wild
                                                         :version :wild
                                                         )
                                          bin-host-pathname)))))))


;;  (probe-file "kr-src:")
;;  ?: (probe-file "kr-src:;")
;;  (probe-file "kr:")

;; (translate-logical-pathname "kr:")
;; ?: (translate-logical-pathname "kr:;")
;; (translate-logical-pathname "kr:kr-compiler.lisp")
;; (translate-logical-pathname "kr:kr-compiler.fasl")
;; (translate-logical-pathname "kr:kr-compiler")


;; (cl-user:garnet-probe "kr-src:kr-compiler" :prefer-source  t)
;; (cl-user:garnet-probe "kr:kr-compiler" )
;; (cl-user:garnet-probe "kr:kr-compiler" :prefer-source t)
;; ^ FIXME: last call should not err


;; **** FIXME NOTE : loading kr-doc
;;
;; note : src/kr/kr-doc.lisp is compiled/loaded individually
;; when compile-kr-doc-p / load-kr-doc-p => t / t
;; per garnet-loader.lisp / garnet-compiler.lisp
;;
;; ****



;; define garnet system directories, for loading the garnet
;; <foo>-loader and <foo>-compiler files
;;
;; garnet-<FOO>-pathname [var] : <FOO> system binary directory
;; garnet-<FOO>-src [var] : <FOO> system source directory
(macrolet ((frob-do (sname)
             (macrolet ((frob-symbol (name)
                          `(let ((s (read-from-string (format nil "#:~a" ,name))))
                             (intern (symbol-name s) '#:cl-user)))
                        (frob-bin-sym (name)
                          `(frob-symbol (format nil "garnet-~a-pathname" ,name)))
                        (frob-src-sym (name)
                          `(frob-symbol (format nil "garnet-~a-src" ,name)))
                        (frob-bin-path (path)
                          `(apply-output-translations
                            (translate-logical-pathname ,path))))
               (let ((foo-bin-var (frob-bin-sym sname))
                     (foo-src-var (frob-src-sym sname))
                     (src-dir (format nil "~a-src:" sname)))
                 `(progn
                    (defvar ,foo-bin-var
                      ,(frob-bin-path src-dir))
                    (defvar ,foo-src-var
                      ,(translate-logical-pathname src-dir))))))
             (frob-toplevel ()
               `(progn
                  ,@(mapcar (lambda (s)  `(frob-do ,s))
                            %garnet-systems%))))
  (frob-toplevel))

;; ^ NB: in SBCL (1.2.3) a (SET NAME VALUE) call may not in itself
;;   register the value of NAME as a defined variable. So, that form
;;   was revised to use a macro definition and DEFVAR instead of SET.

;; (describe 'cl-user::garnet-kr-pathname)

;; --

(defclass garnet-proxy-system (system)
  ())

(defmethod operate ((op compile-op) (component garnet-proxy-system)
                    &key)
  (let* ((name (component-name component))
         (compiler-file-name (format nil "~a-src:~:*~a-compiler.lisp" name))
         (system-compiler-file (cl-user:garnet-probe compiler-file-name
                                                     :prefer-source t)))
    (with-compilation-unit
        (:policy cl-user::*default-garnet-proclaim*)
      (cl-user:garnet-load system-compiler-file))))

;; FIXME: ensure that COMPILE-OP calls LOAD-OP on same component
;;        after compiling

(defmethod operate ((op load-op) (component garnet-proxy-system)
                    &key)
  (let* ((name (component-name component))
         (loader-file-name (format nil "~a-src:~:*~a-loader.lisp" name))
         (system-loader-file (cl-user:garnet-probe loader-file-name )))
    (cl-user:garnet-load system-loader-file)))


(defmethod component-depends-on ((op load-op)
                                 (component garnet-proxy-system))
  (list (list 'compile-op component)))

(do ((systems (reverse %garnet-systems%)
              (cdr systems)))
    ((null systems) t)
  (let ((sysname (car systems))
        (previous (cadr systems)))
    (register-system-definition
     sysname
     :serial t
     :class 'garnet-proxy-system
     :depends-on (when previous (list previous))
     :pathname *load-truename*)))

;; NB: kr-loader may assume kr-compiler has been run previously

;; (asdf:operate 'asdf:compile-op '#:kr)
;; (asdf:operate 'asdf:load-op '#:kr)
;; ^ TESTS

;; (require '#:clx)
;; (dolist (s %garnet-systems%) (asdf:operate 'asdf:compile-op s))
;; ^ NOTE: The truetype <thing> loads additional system definitions

;; NB: gadgets:GAD-scroll-parts.lisp is not missing
;; but gadgets-compiler.lisp was not finding it
;;     during garnet-load
;;         during load of gadgets-compiler.lisp (within COMPILE-OP)


;;; source: garnet-compiler.lisp
(defpackage :GARNET-UTILS (:use :COMMON-LISP) (:nicknames :GU))
(defpackage :KR-DEBUG (:use :COMMON-LISP))
(defpackage :KR (:use :COMMON-LISP :KR-DEBUG))
(defpackage :GEM (:use :COMMON-LISP :KR :KR-DEBUG))
(defpackage :OPAL (:use :COMMON-LISP :KR))
(defpackage :INTERACTORS (:use :COMMON-LISP :KR) (:nicknames :INTER)
            (:export *GARNET-BREAK-KEY* *LEFT-BUTTON* *TRANS-FROM-FILE*))
(defpackage :GARNET-GADGETS (:use :COMMON-LISP :KR) (:nicknames :GG))
(defpackage :GARNET-DEBUG (:use :COMMON-LISP :KR :OPAL) (:nicknames :GD))
(defpackage :GILT (:use :COMMON-LISP :KR))
(defpackage :C32 (:use :COMMON-LISP :KR))
(defpackage :LAPIDARY (:use :COMMON-LISP :KR))
(defpackage :AGATE (:use :COMMON-LISP :KR))

(defpackage :DEMO-3D (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-MULTIWIN (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
(defpackage :DEMO-MULTIFONT (:use :COMMON-LISP KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-ANIMATOR (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-ANGLE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
(defpackage :DEMO-OTHELLO (:use :KR :COMMON-LISP) (:nicknames :DOTH)
            (:export DO-GO DO-STOP START-GAME STOP-GAME SET-SCORE))
(defpackage :DEMO-PIXMAP (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-ARITH (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
(defpackage :DEMO-SCHEMA-BROWSER (:use :COMMON-LISP :KR)
            (:export DO-GO DO-STOP SCHEMA-BROWSER SCHEMA-BROWSER-WIN
                     SCHEMA-BROWSER-TOP-AGG))
(defpackage :DEMO-ARRAY (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-SCROLLBAR (:use :COMMON-LISP :KR)
            (:export DO-GO DO-STOP
                     MAC-obj MAC-Go MAC-Stop
                     Open-obj Open-Go Open-Stop
                     NEXT-obj NEXT-Go NEXT-Stop
                     Motif-obj Motif-Go Motif-Stop))
(defpackage :DEMO-CLOCK (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
(defpackage :DEMO-SEQUENCE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-EDITOR (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
(defpackage :DEMO-TEXT (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-FILE-BROWSER (:use :COMMON-LISP :KR)
            (:export DO-GO DO-STOP FILE-BROWSER FILE-BROWSER-WIN
                     FILE-BROWSER-TOP-AGG))
(defpackage :DEMO-TRUCK (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
(defpackage :DEMO-GADGETS (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-TWOP (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
(defpackage :DEMO-GESTURE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
(defpackage :DEMO-UNISTROKES (:use :COMMON-LISP :KR :INTER) (:export DO-GO DO-STOP))
(defpackage :DEMO-GRAPH (:use :COMMON-LISP :KR)
            (:export DO-GO DO-STOP SCHEMA-GRAPH DEMO-GRAPH-ERROR-GADGET ROOT-BOX
                     RELAYOUT DEMO-GRAPH-WIN))
(defpackage :DEMO-VIRTUAL-AGG (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-GROW (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
(defpackage :DEMO-XASPERATE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-LOGO (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP RE-ANIMATE))
(defpackage :DEMOS-CONTROLLER (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-MANYOBJS (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-MENU (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :GARNET-CALCULATOR (:use :COMMON-LISP :KR)
            (:export START-CALC STOP-CALC DO-GO DO-STOP))
(defpackage :DEMO-MODE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :GARNETDRAW (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :DEMO-MOTIF (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
(defpackage :MGE (:use :COMMON-LISP :KR)
            (:export DO-GO DO-STOP
                     CREATE-PIECE DESTROY-PIECE DESTROY-ALL-PIECES
                     GO-INITIALIZE EDITOR-SHOW-WINDOW))
(defpackage :DEMO-MOVELINE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))

;; prototype code

;; (let ((cl-user-pkg (find-package '#:cl-user)))
;;   ;; derive components (TO DO)
;;   (dolist (s %garnet-systems%)
;;     (asdf:operate 'asdf:compile-op s)
;;     (let ((files-list-sym
;;            (intern
;;             (symbol-name
;;              (read-from-string (format nil "#:garnet-~a-files" s)))
;;             cl-user-pkg))
;;           (system (asdf:find-system s))
;;           (system-src-path
;;            (translate-logical-pathname
;;             (format nil "~s-src:" s))))
;;       (setf (asdf:module-components system)
;;             (mapcar
;;              (lambda (name)
;;                (make-instance 'asdf:cl-source-file
;;                               :name name
;;                               :pathname
;;                               (make-pathname :name name
;;                                              :type "lisp"
;;                                              :defaults
;;                                              system-src-path)))
;;              (or (symbol-value files-list-sym)
;;                  (error
;;                   "No files found in system ~s (files list ~s)"
;;                   s files-list-sym)))))))
