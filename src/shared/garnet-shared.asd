;; garnet-shared.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:garnet-systems
    (:use #:asdf #:cl))

  (setf *features*
	(pushnew ':garnet.asdf *features*
		 :test #'eq))

  (in-package #:garnet-systems)  

  (asdf:operate 'asdf:load-op 
		'#:info.metacommunity.cltl.utils)
  )


(in-package #:garnet-systems)


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


(defclass garnet-source-file (cl-source-file)
  ())


(defmethod asdf:operate :around ((op compile-op) (c garnet-source-file) &key)
  (with-compilation-unit
      (:policy cl-user::*default-garnet-proclaim*)
    (call-next-method)))


(defmacro with-safe-frefs (specs &body body)
  ;; NB : "Not applicable" for (SETF FOO) function names
  (let ((%s (gentemp "%s-"))
	(%foundp (gentemp "%foundp-")))
    (flet ((s-name (s)
	     (etypecase s
	       (string s)
	       (symbol (symbol-name s))
	       (character (string s))))
	   #+UNUSED_CODE
	   (name-s (s pkg)
	     (declare (type string s))
	     (intern s pkg))
	   (pkg-name (p)
	     (etypecase p
	       (package (package-name p))
	       (symbol (symbol-name p))
	       ((or string character)
		(values p)))))
      `(let (,@(mapcar (lambda (spec)
			 (destructuring-bind 
			       (name fn &optional 
				     (package
				      (etypecase fn
					(symbol
					 (or (symbol-package fn)
					     *package*))
					((or string character)
					 *package*))))
			     spec
			   (let ((fname (s-name fn))
				 (pkgname (pkg-name package)))
			     `(,name
				(multiple-value-bind (,%s ,%foundp)
				    (find-symbol ,fname
						 ,pkgname)
				  (cond
				    (,%foundp
				     (values (fdefinition ,%s)))
				    (t
				     (error "Package ~A does not contain a symbol ~S"
					    (find-package ,pkgname)
					    (quote (quote ,fname))))
				    ))))))
		       specs))
	 ,@body))))

;; (macroexpand-1 '(with-safe-frefs ((l list)) (funcall l 1 2)))
;; (with-safe-frefs ((l list)) (funcall l 1 2))
;;; => (1 2)

#+NIL 
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

(defsystem #:garnet-shared
  :defsystem-depends-on (#:info.metacommunity.cltl.utils)
  :default-component-class garnet-source-file
  :components ((:file "shared")))
