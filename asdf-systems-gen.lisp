;;
;; asdf-systems-gen.lisp - generate ASDF system definitions
;;                         for Garnet systems

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
    #-(or allegro CMU) "truetype"
    ;; ^ why is truetype-compiler spec #-(or allegro CMU) in garnet-compiler.lisp?
    ;; note also: the truetype compiler file includes a loop calling 'require'
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

;; Legacy Garnet Pathname Variables

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
  ;; New feature in Garnet API
  (let* ((pathname-with-type
          (merge-pathnames
           pathname
           (make-pathname :type (if prefer-source
                                    source-type
                                    #.(pathname-type (compile-file-pathname #"foo"))))))
         (xl (translate-logical-pathname pathname-with-type)))
    (or (probe-file xl)
        (let ((dest
               (cond
                 (prefer-source
                  (make-pathname :type source-type :defaults xl))
                 (t
                  (make-pathname
                   :type #.(pathname-type  (compile-file-pathname "foo"))
                   :defaults
                   (asdf:apply-output-translations xl))))))
          (or (probe-file dest)
              (when errorp
                (error "Cannot locate file ~s (tried paths ~s and ~s)"
                       pathname xl dest)))))))



;; (garnet-probe "kr-src:kr-macros" :prefer-source t)
;; (garnet-probe "kr:kr-macros"  :prefer-source t)

;; Legacy Garnet System Functions

(defun garnet-load (pathname)
  ;; frob, nothing with regards to garnet-load-alist
  (let ((file (garnet-probe pathname)))
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

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf sb-ext:*muffled-warnings* 'sb-kernel::style-warning))

#+NIL
(when *default-garnet-proclaim*
  (proclaim *default-garnet-proclaim*))

;; -- features and legacy variables, directly from garnet-loader.lisp

(defparameter Garnet-Version-Number "3.3")
(pushnew :GARNET *features*)
(pushnew :GARNET-V3 *features*)
(setf *features* (delete :GARNET-V3.0 *features*))
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


;; --------

(in-package #:garnet-systems)

(dolist (sys %garnet-systems%)
  ;; define logical pathnames
  ;;   for <foo>-src: LPN hosts
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
                  (list (list "**;*.*.*"
                              (merge-pathnames (make-pathname :directory
                                                              '(:relative :wild-inferiors)
                                                              :name :wild
                                                              :type :wild
                                                              :version :wild
                                                              )
                                               src-host-pathname)))))

    (setf (logical-pathname-translations bin-host-name)
          (append lpn-translations
                  (list (list "**;*.*.*"
                              (merge-pathnames (make-pathname :directory
                                                              '(:relative :wild-inferiors)
                                                              :name :wild
                                                              :type :wild
                                                              :version :wild
                                                              )
                                               bin-host-pathname)))))))


;;  (probe-file "kr-src:")
;;  (probe-file "kr:")

;; (translate-logical-pathname "kr:")
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
;;
;; ****



;; define garnet legacy system directories, for loading the legacy
;; <foo>-loader and <foo>-compiler files
;;
;; garnet-<FOO>-pathname [var] : <FOO> system binary directory
;; garnet-<FOO>-src [var] : <FOO> system source directory
(labels ((frob-symbol (name)
           (let ((s (read-from-string name)))
             (prog1 (intern (symbol-name s) '#:cl-user)
               (unintern s))))
         (frob-bin-sym (name)
           (frob-symbol (format nil "garnet-~a-pathname" name)))
         (frob-src-sym (name)
           (frob-symbol (format nil "garnet-~a-src" name)))
         (frob-bin-path (path)
           (apply-output-translations
            (translate-logical-pathname path))))
  (dolist (s %garnet-systems%)
    (let ((foo-bin-var (frob-bin-sym s))
          (foo-src-var (frob-src-sym s))
          (dir (format nil "~a-src:" s)))
      (unless (boundp foo-bin-var)
        (setf (symbol-value foo-bin-var)
              (frob-bin-path dir)))
      (unless (boundp foo-src-var)
        (setf (symbol-value foo-src-var)
              (translate-logical-pathname dir))))))


;; --

(defclass garnet-proxy-system (system)
  ())

(defmethod operate ((op compile-op) (component garnet-proxy-system)
                    &key)
  (let* ((name (component-name component))
         (compiler-file-name (format nil "~a-src:~:*~a-compiler.lisp" name))
         (system-compiler-file (or (cl-user:garnet-probe compiler-file-name
                                                         :prefer-source t)
                                   (error "File not found: ~s"
                                          compiler-file-name))))
    (with-compilation-unit
        (:policy cl-user::*default-garnet-proclaim*)
      (load system-compiler-file))))

;; FIXME: ensure that COMPILE-OP calls LOAD-OP on same component
;;        after compiling

;; FIXME: define COMPILE-OP as prereq to LOAD-OP on GARNET-PROXY-SYSTEM

(defmethod operate ((op load-op) (component garnet-proxy-system)
                    &key)
  (let* ((name (component-name component))
         (loader-file-name (format nil "~a-src:~:*~a-loader.lisp" name))
         (system-loader-file (or (cl-user:garnet-probe loader-file-name )
                                 (error "File not found: ~s"
                                        loader-file-name))))
    (load system-loader-file)))



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

;; FIXME: kr-loader assumes that kr-compiler has been run previously
;; (asdf:operate 'asdf:compile-op '#:kr)
;; ^ TEST

;;; FIXME: ALSO DEFPACKAGE.

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
