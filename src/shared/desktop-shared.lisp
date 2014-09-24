;; desktop-shared.lisp - shared resources for Garnet desktop applications

;; FIXME: SIDEBAR : What feature GARNET-PROCESSES ?

;; FIXME: Parse source code for each of:
;;  Garnet-Bitmap-Pathname
;;  Garnet-Pixmap-Pathname [X] [used in gilt and demos systems][X]
;;  Garnet-Gilt-Bitmap-Pathname
;;  Garnet-C32-Bitmap-Pathname
;;  Garnet-DataFile-Pathname
;;  Garnet-Gesture-Data-Pathname
;;
;; ensuring:
;;  #+Garnet.ASDF
;;    (find-bitmap-pathname <name> :system <system>)

;; FIXME: Sysem deps
;;
;;
;;  A. Depending on garnet-bitmaps :
;;       garnet-demos (incl. stand-alone garnetdraw system)
;;       garnet-gadgets (various specific gadgets)
;;       garnet-opal
;;       garnet-lapidary
;;       garnet-debug
;;  B. Depending on garnet-pixmaps :
;;       garnet-demos (...)
;;       garnet-gilt
;;       garnet-debug



;; FIXME: cf. cl-user::Garnet-Bitmap-Pathname in legacy Garnet system definitions
;; e.g systems using definition of that var:
;;  garnet-demos [FIXME: use GET-GARNET-BITMAP]
;;  garnet-gadgets (used with OPAL:READ-IMAGE) [FIXME: use GET-GARNET-BITMAP]
;;  garnet-opal (used by GET-GARNET-BITMAP [FIXME: revise pathname eval, move into GADGETS]
;;  garnet-lapidary (cf. LAPIDARY::INIT-CURSORS) [FIXME: use GET-GARNET-BITMAP]

(in-package #:garnet-systems)

;; for a Garnet bitmap named <name>
;; one file exists: <name>.bm
;;
;; for a Garnet cursor named <name>,
;; two files exist:
;;    <name>.cursor
;;    <name>.mask
;; both files, in bitmap format



(export '(bitmap cursor cursor-cursor-bitmap cursor-mask-bitmap
          image
          find-bitmap-pathname
          ))


;;; PIXMAP component class

(defclass pixmap (resource-file)
  ())

(defmethod source-file-type ((component pixmap) (context parent-component))
  (let ((type (call-next-method*)))
    (or type "xpm")))


;;; BITMAP Component Class

(defclass bitmap (resource-file)
  ())

(defmethod source-file-type ((component bitmap) (context parent-component))
  (let ((type (call-next-method*)))
    (or type "bm")))


;;; CURSOR Component Class

(defclass cursor (resource parent-component)
  ;: A cursor, in Garnet -- certainly, in a manner as would similar as
  ;; to CLX and braoder XLIB -- for a cursor's effective storage  in
  ;; the filesystem, a cursor is comprised of an X _bitmap_
  ;; resource and an X _bitmap mask_ resource, each being
  ;; represented of an individual file in the filesystem.
  ;;
  ;; This class specifies a single CURSOR component class
  ;; for ASDF, such that this CURSOR component class
  ;; will represent both file resouces, effectiely.
  ;;
  ;; A corresponding interface is defined in the function,
  ;; FIND-BITMAP-PATHNAME such as to utilize this extension
  ;; on ASDF for purpose of locating cursor, bitmap, and pixmap
  ;; resources within the filesystem.
  ((cursor-bitmap
    :initarg :cursor-bitmap
    :type component
    :accessor cursor-cursor-bitmap)
   (mask-bitmap
    :initarg :mask-bitmap
    :type component
    :accessor cursor-mask-bitmap)
   ))

#+NIL ;; may be redundant, or overriden by other code in ASDF
(defmethod component-pathname :around ((component cursor))
  ;; CURSOR is a component with no single source-file mapping.
  ;;
  ;; As similiar to conventional MODULE components, the pathname
  ;; of a CURSOR will nonetheless establish the pathname of
  ;; the effective components of a CURSOR
  ;;
  ;; If a :pathname was specified to the component when the object
  ;; was initialized, then this method SHOULD revert to that pathname,
  ;; effectively.
  ;;
  ;; Otherwise, this method SHOULD revert to the pathname
  ;; of the containing component, if applicable.
  (let* ((parent (component-parent component))
         (*default-pathname-defaults*
          (cond
            (parent
             (component-pathname parent))
            (t *default-pathname-defaults*)))
         (p (when (next-method-p)
              (call-next-method))))
    ;; BORKED.
    #+NIL (warn "PING: ~s (~s) ~s" component parent p) ;; debug
    (or p
        (when parent
          *default-pathname-defaults*))))

(defmethod component-children ((component cursor))
  (let ((cursor (cursor-cursor-bitmap component))
        (mask (cursor-mask-bitmap component)))
    (list* cursor mask (call-next-method*))))

(defmethod module-components ((component cursor))
    ;; "Backwards compat"
  (component-children component))



(defmethod shared-initialize :around ((instance cursor) slot-names
                                      &rest initargs
                                      &key pathname
                                        parent
                                        &allow-other-keys)
  (when (and (or (eq slot-names t)
                 (find 'asdf::relative-pathname slot-names :test #'eq))
             (not pathname))
    ;; Cannot (SETF COMPONENT-PATHNAME) [ASDF 3.1.3.8]
    ;;
    ;; So, to set the component's pathname to a custom value,
    ;; simply a "hack" onto the initargs...
    (setf (getf initargs :pathname)	  
	  (cond
	    (parent (component-pathname parent))
	    (t (make-pathname  :directory '(:relative))))))

  (when (next-method-p)
    (apply #'call-next-method instance slot-names initargs))


  (macrolet ((default (arg value)
               (let ((%arg `(quote ,arg)))
                 `(when (or (eq slot-names t)
                            (member ,%arg slot-names :test #'eq))
                    (unless (slot-boundp instance ,%arg)
                      (setf (slot-value instance ,%arg)
                            ,value)))))
             (default-bm (slot-name type obj-name instance)
               (let ((%cbm-path (gentemp "%cbm-path-"))
                     (%cname (gentemp "%cname-")))
                 `(default ,slot-name
                      (let ((,%cname (format nil "~A-~A"
                                             (asdf:coerce-name ,obj-name)
                                             (asdf:coerce-name
                                              (quote ,slot-name))))
                            (,%cbm-path
                             (make-pathname :name ,obj-name
                                            :type ,type
                                            :defaults
                                            (component-pathname ,instance))))
                        (make-instance 'bitmap
                                       :name ,%cname
                                       :pathname ,%cbm-path
                                       :parent ,instance))))))

    (let ((obj-name (component-name instance))
          (rpath (component-pathname instance)))
      (when obj-name
        (cond
          (rpath
           (default-bm cursor-bitmap "cursor" obj-name instance)
           (default-bm mask-bitmap "mask" obj-name instance))
          (t (flet ((check (slot)
                      (unless (slot-boundp instance slot)
                        (simple-style-warning
                         "~<Unable to define default value for ~S slot ~S~>~%
~<Component pathname not avaialble for ~0@*~S~>"
                         instance slot))))
               (check 'cursor-bitmap)
               (check 'mask-bitmap))))))))



;;; NOTES

#|

FIXME: Define a "Resource System" compression framework, for
application when installing a "Resource system" with Common
Lisp, onto a thin client mobile device.

--

## Towards A concept: "Resource system"

example: The garnet-bitmaps system (defined heredin)

### Summary:

A Common Lisp application may be defined, such that -- for the
application's normal functioning -- would require resources not
expressly representing Common Lisp source files. For example,
a "local dictionary" application would need to access
"local" dictionary data files. Similary, a graphical user application
such as Garnet Draw would require access to bitmap files or other
image files used in the application's graphical user interface.

(If "resource system" may be defined expressly as a class ... ?)

--

## "Origins"

Initial Question: How to derive CL-USER::GARNET-BITMAP-PATHNAME  from
this system definition, portably?


* Note the variable cl-user::Garnet-Bitmap-Pathname
is used in current revisiosn of Garnet source tree -- eg. within
the function, GET-GARNET-BITMAP -- and as used within some
demonstration programs -- for instance, Garnet Draw. That variable,
essentially, must define the root directory in which the Garnet
system's bitmap files are stored


* In a revision of Garnet using ASDF, CL-USER::GARNET-BITMAP-PATHNAME
can be bound to the source pathname of the GARNET-BITMAPS
system. However, that would prevent compatibility onto any
non-source distributions of the Garnet system.

How then may one determine when to define GARNET-BITMAP-PATHNAME
1. using the SOURCE-PATHNAME of the GARNET-BITMAPS system?
2. or using ASDF:APPLY-OUTPUT-TRANSLATIONS onto the GARNET-BITMAPS sytsem?

* That may be resolvable with a simple <PLATFORM>//<INCLUDE-SOURCE-P>
property, such as may be defined as a "configuration property,"
in "some sort of a lexical environment." That, in itself, may not
serve to answer a broader question, however,

"Why would it matter?"

Towards such a question, the following sections might serve to
provide something in a form of an explanation.

* Towards a second question, "How would that be implemented?"

some ideas:

* <PLATFORM>//<INCLUDE-SOURCE-P>

* refers to ASDF system definitions, specifically those
representing "Resource systems" within any single
application

* may be referenced from "platform builder" programs

* would be expressly relevant for appliation functioning,
as with regards to how that 'property' would affect
the availability of <resource systems> in a target
<platform>

* IDE <=> Source distribution management tooling

* Source distribution management tooling in Common Lisp?
* xref: Eclipse IDE (Java) [Platform Element]
* xref: Eclipse IDE for embedded systems [!] [Platform Element]
* xref: Dandelion extension for the Eclipse IDE [Platform Element]


* Concept: User/developer option, "Install system source code?"


* Sidebar: Referencing TimeSource LinuxLink [Existing Work]

* Concept: RTOS and thin client embedded systems development
* Concept: Extending on existing RTOS developments
in the Linux Kernel and GNU LibC

* Development environment <=> device image builder
* Kernel
* RootFS
* Platform Libraries

* Concern: Minimization of file size of compiled binary files -
e.g. using GCC or uClibc for C programs - and tradeoffs
with regards to minimizing "compiled binary file size"

* Concern: Optimization for target architectures, in a
platform-centric regards

* Sidebar: compressed filesystems may or may not be "ideal"
for realtime architectures (e.g due to latency across the
                                filesystem compression interface) ?
--

## Towards a Common Lisp Installation for a Graphing Calculator

* Conceivably, for a use case in which Garnet could be used to define
a graphical user interface system for a Common Lisp implementation
installed onto a portable calculator device, "Then it would matter",
as namely: That any Common Lisp systems installed onto such a thin
client platform may not be installed with their source forms.
Rather, a "Resource system" such as the garnet-bitmaps system may
be installed into some sort of a compressed form - to be
decompressed at system runtime - without the corresponding source
code of the system then being available on the calculator, itself.

* In short, then, this may serve to call towards a matter of defining
a "System compression" and related "Component compression" model for
Common Lisp systems using ASDF, ostensibly for application on
thin-client platforms - e.g. CCL for the Advanced RISC Machine (ARM)
architecture.

----

As a sidebar; The Texas Instruments nSpire platform,
with some effort, could possibly serve as a demonstration
platform, a sort of "proof of concept" of a kind, for
"Common Lisp on a Calculator". Such a platform would
then allow for extension of the baseline calculator
platform, using Common Lisp. (Comparatively, in a sense,
                                             the TI nSpire platform supports user extension with
                                             Lua scripting. Lua is also used in the TOME 4 grapical
                                             adventure game platform, incidentally)

|#


;;; FIND-BITMAP-PATHNAME


(deftype image ()
  ;; cf. GEM::X-READ-AN-IMAGE
  #-:xlib t
  #+:xlib
  '(or xlib:image-z xlib:image-xy xlib:image-x))



(defun find-bitmap-pathname (name &key (errorp t)
                                    (system "garnet-bitmaps")
                                    submodule)
  ;; FIXME: should rename this function, across the source tree
  ;; => find-resource-pathname
  ;; wheras this function applies both for bitmaps and cursors now
  ;;
  ;; FIXME: Must also generalize this for application e.g. onto the Gilt
  ;; resource system (bitmaps, pixmaps)
  ;;
  ;; Therefore: TO DO
  ;;  1) [X] Move this into a (new) garnet-desktop-shared system
  ;;  2) [X] Add a keyword arg :SYSTEM
  ;;  3) Use for all of the systems:
  ;;       [X] garnet-bitmaps
  ;;       [X] garnet-pixmaps
  ;;       [x] garnet-c32-resources
  ;;       garnet-gesture-resources [TO DO] (*.classifier ?)
  ;;       garnet-gilt-resources [TO DO]
  ;;       ?? ../data/cirles.data used in demo-virtual-agg [TO DO]
  (declare (type string name)
           (type (or null string) submodule)
           (values (or pathname null)
                   (or pathname null)))
  (let* ((%name (asdf:coerce-name name))
         (sys (utils:find-component* system nil))
         (c (cond
              (submodule
               (let* ((%submodule (asdf:coerce-name submodule))
                      (module (utils:find-component* %submodule sys)))
                 (utils:find-component* %name module nil)))
              (t (utils:find-component* %name sys nil)))))
    (cond
      (c
       (typecase c
         (cursor
          (macrolet ((get-path (property obj)
                       `(asdf:component-pathname (,property ,obj))))
            (values
             (get-path cursor-cursor-bitmap c)
             (get-path cursor-mask-bitmap c))))
         (t (values (asdf:component-pathname c) nil))))
      (errorp
       (simple-program-error
        "Bitmap resource system ~S does not contain a bitmap named ~S"
        system %name ))
      (t (values nil nil)))))


;;; Misc tests

;; (find-bitmap-pathname "downarrow")

;; (find-bitmap-pathname "garnet")

;; (find-bitmap-pathname "line" :submodule "garnetdraw")



;; (asdf:module-components (find-component* "garnet" "garnet-bitmaps"))
;;
;; (describe (find-component* "garnet" "garnet-bitmaps"))
#+NIL
(defun enumerate-slot-values (instance)
  (let* ((c (class-of instance)))
    (mapcar #'(lambda (sl)
                (cons
                 (c2mop:slot-definition-name sl)
                 (when (c2mop:slot-boundp-using-class c instance sl)
                   (list (c2mop:slot-value-using-class c instance sl)))))
            (c2mop:class-slots c))))
;;
;; (enumerate-slot-values (find-component* "garnet" "garnet-bitmaps"))
;; ^ CHILDREN and CHILDREN-BY-NAME slots are empty. why?


;; (component-pathname (cursor-cursor-bitmap (find-component* "garnet" "garnet-bitmaps")))
