;; desktop-shared.lisp - shared resources for Garnet desktop applications
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

;; NB: This file represents something of a prototype towards a
;; component-oriented representation of application resources in Garnet,
;; focusing mainly on Garnet Bitmap, Pixmap, and Icon resources. An
;; effort is made for developing a reusable class-oriented
;; representation of such resources, for their representation in
;; individual, digitally encoded filesystem objects and towards their
;; representation in KR and broader Garnet schema objects,
;;
;;
;; Commentary - The Garnet-Bitmaps System
;;
;; [T.D - cf. *.asd files]
;;
;; Not as-yet addressed here:
;;
;; - Portability. This edition requires a single generic function and a
;;   number of methods specialized onto that generic function, such as
;;   are provided via an externally linked system - presently, the
;;   ltp-asdf-utils system. That system has been published in the
;;   Thinkum Labs Lisp Tools Project (LTP) source repository.
;;
;;   This external linkage has been viable, if only insofar as for
;;   purposes of prototyping. This external linkage should be removed,
;;   before any formal release of this system.
;;
;; - XDG Application Directories, for Desktop Environments - not, as
;;   yet, represented in this system. Such representation must
;;   necessarily entail a linkage onto implementation-specific features
;;   such as for interfacing onto platform APIs for shell commands,
;;   process objects, PTYs and file streams -- principally vis a vis
;;   standard POSIX APIs, if not also in reference to the X/Open Single
;;   Unix Specifications, up to and including SUSv4.
;;
;; - Device-Independent Representation of Static Graphics Resources
;;
;;   The nearest sense of device-independent abstraction that this
;;   system approaches is entailed in a manner of representation of
;;   application resources as generic filesystem objects, principally in
;;   the PIXMAP and CURSOR component classes. In the instance of the
;;   CURSOR class, this component class represents something of an
;;   aggregate class onto a cursor bitmap and a cursor mask --
;;   insofar as such resources may be represented, each, with a static
;;   filesystem object.
;;
;;   The representation in this system is developed as independent of
;;   any application of such resources.
;;
;;   In the case of a cursor object, the resources may be used
;;   principally for image compositing in the X window system, namely
;;   for image transparency in the cursor object. Theoretically -- as
;;   via Garnet -- this should also be portable onto the Garnet gworld
;;   system.
;;
;;   In order to retain separation to the application, this system does
;;   not in itself utilize any features of the Garnet KR system, in this
;;   present revision.
;;
;; [spchamp - Nov 2018]

;; NB #+GARNET-PROCESSES cf. garnet-loader.lisp and ./shared.lisp
;; b.c INIT-OP (TO-DO)

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
;;    (bitmap-pathnames <name> :system <system>)

;; FIXME: System deps
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

;; FIXME: cf. cl-user::Garnet-Bitmap-Pathname in original Garnet system definitions
;;
;; e.g systems
;;  garnet-demos [FIXME: use GET-GARNET-BITMAP]
;;  garnet-gadgets (used with OPAL:READ-IMAGE) [FIXME: use GET-GARNET-BITMAP]
;;  garnet-opal (used by GET-GARNET-BITMAP [FIXME: note pathname eval and GADGETS system deps??]
;;  garnet-lapidary (cf. LAPIDARY::INIT-CURSORS) [FIXME: use GET-GARNET-BITMAP]

(in-package #:garnet-sys)

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
          bitmap-pathnames
          ))


;; --- cf CMUCL

(define-condition simple-program-error (program-error simple-condition)
  ())

(define-condition simple-style-warning (style-warning simple-condition)
  ())

;; ----

(defclass resource (component)
  ;; NB abstract component class
  ())

(defclass resource-module (resource module)
  ;; NB: XDG APP DIRS
  ())


(defclass resource-system (resource-module system)
  ;; NB: XDG APP DIRS
  ())

(defclass resource-file (resource source-file)
  ())

(defmethod perform ((op compile-op) (component resource-file))
  (declare (ignore op component))
  ;; no-op
  )

(defmethod perform ((op load-op) (component resource-file))
  (declare (ignore op component))
  ;; no-op
  )

(defmethod perform ((op load-source-op) (component resource-file))
  (declare (ignore op component))
  ;; no-op
  )

#+CBSE-INSTALL
(defmethod perform ((op install-bytecode-op) (component resource-file))
  ;; TBD onto XDG APP DIRS
  ;;
  ;; TBD :IF-EXISTS :error-if-checksum-mismatch
  )

#+CBSE-INSTALL
(defmethod perform ((op install-source-op) (component resource-file))
  ;; TBD onto XDG APP DIRS
  ;;
  ;; TBD :IF-EXISTS :error-if-checksum-mismatch
  )


(defmethod asdf/component:module-default-component-class
    ((module resource-module))
  (find-class 'resource-file))

;; ltp-utils::call-next-method*

(defmacro call-next-method* (&rest args)
  ;;; ported from ltp-utils
  (let ((%nmp (make-symbol "%nmp")))
    `(let ((,%nmp (next-method-p)))
       (cond
	 (,%nmp (call-next-method ,@args))
	 (t (values nil nil))))))

;; ----

;;; PIXMAP component class

(defclass pixmap (resource-file)
  ;; e.g *.xpm
  ;;
  ;; NB also *.bm files in garnet:lib;c32; - appears to be an XPM format
  ())

(defmethod source-file-type ((component pixmap) (context parent-component))
  (let ((type (call-next-method*)))
    (or type "xpm")))


;;; BITMAP Component Class

(defclass bitmap (resource-file)
  ;; FIXME This could share a common class onto PIXMAP other than per RESOURCE-FILE
  ())

(defmethod source-file-type ((component bitmap) (context parent-component))
  (let ((type (call-next-method*)))
    (or type "bm")))


;;; CURSOR Component Class

(defclass cursor (resource parent-component)
  ;; FIXME: It may be feasible to use a common superclass onto PIXMAP,
  ;; other than the generic RESOURCE class -- if only for purpose of
  ;; representing, in this system, any "Application file" characteristics
  ;; of cursor and pixmap resources, by-in-large independent of resource
  ;; interface implementations.
  ;;
  ;; NB: It may serve to facilitate application onto ASDF pathname
  ;; transformations, to use a single GRAPHICS-RESOURCE superclass.
  ;;
  ;; Gesture definitions would not use the GRAPHICS-RESOURCE class, of
  ;; course.

  ;; A cursor, in Garnet -- certainly, in a manner as may be similar to
  ;; to CLX and broader XLIB -- for a cursor's effective storage in
  ;; the filesystem, a cursor may be comprised of an X _bitmap_
  ;; resource and an X _bitmap mask_ resource. Typically, in Garnet,
  ;; each such resource would be represented with an individual file in
  ;; the filesystem.

  ;; This class specifies a single, aggregate CURSOR component class
  ;; for ASDF, such that an instance of this CURSOR component class
  ;; will be representative of both file resources -- each, represented
  ;; with an ASDF COMPONENT object, in the aggregate CURSOR object. This
  ;; is for providing application data for the CURSOR presentation.
  ;;
  ;; In the definition developed here, a CURSOR does not per se comprise
  ;; an ASDF module, considering that the bitmap and mask resources may
  ;; not need to be individually evaluated for most ASDF:PERFORM
  ;;
  ;; A corresponding interface is defined in the function,
  ;; BITMAP-PATHNAMES such as to utilize this extension
  ;; on ASDF for purpose of locating cursor, bitmap, and pixmap
  ;; resources within the filesystem.

  ;; NB: Note the significance of the COMPONENT-PATHNAME for the PIXMAP
  ;; component, when deriving the component pathname for each of the
  ;; CURSOR-BITMAP and MASK-BITMAP in the method onto SHARED-INITIALIZE,
  ;; defined below. Note also that those default-value setters do not
  ;; set a value if a value was already set in SHARED-INITIALIZE
  ((cursor-bitmap
    :initarg :cursor-bitmap
    ;; NB typically :type bitmap
    :type component ;; FIXME see following comments
    ;; :type (or component pathname) ;; NB - see SHARED-INITIALIZE method, subsq.
    ;;
    ;; NB It may be simpler to just store a pathname, not a complete
    ;; component object, in each of these aggregate component slots.
    ;;
    ;; Insofar as this system may be assume to not have been distributed
    ;; for reuse, should then be able to just use :type pathname
    ;;
    ;; :type pathname
    :accessor cursor-cursor-bitmap)
   (mask-bitmap
    :initarg :mask-bitmap
    ;; NB typically :type bitmap - see below
    :type component
    ;; :type (or component pathname)
    ;; :type pathname
    :accessor cursor-mask-bitmap)
   ))

#+NIL ;; may be redundant
(defmethod component-pathname :around ((component cursor))
  ;; CURSOR is a component with no single source-file mapping.
  ;;
  ;; As similar to conventional MODULE components, the pathname
  ;; of a CURSOR may nonetheless establish a base pathname to the
  ;; pathname of the effective components of a CURSOR
  ;;
  ;; If a :pathname was specified to the component when the object
  ;; was initialized, then this method SHOULD revert to that pathname -
  ;; such that this method presumes to do via the next method.
  ;;
  ;; Otherwise, this method SHOULD revert to the pathname
  ;; of the containing component, if applicable.
  (let ((p (when (next-method-p)
             (call-next-method))))
    #+NIL (warn "Debug: ~s (~s) ~s" component parent p)
    (cond
      (p p)
      (t
       (let ((parent (component-parent component)))
         (or (when parent (component-pathname parent))
             *default-pathname-defaults*))))))

(defmethod component-children ((component cursor))
  (let ((cursor (cursor-cursor-bitmap component))
        (mask (cursor-mask-bitmap component)))
    (list* cursor mask (call-next-method*))))

(defmethod module-components ((component cursor))
  ;; "Backwards compat" (??)
  ;; or cross-wise compat. onto the ASDF 3 API
  (component-children component))


(defmethod shared-initialize :around ((instance cursor) slot-names
                                      &rest initargs
                                      &key pathname
                                        parent
                                        &allow-other-keys)
  ;; NB: This uses a :PATHNAME initarg to the CURSOR class,
  ;;     then deriving a behavior for defaulting of the value accessed
  ;;     by ASDF::COMPONENT-RELATIVE-PATHNAME onto a slot such that
  ;;     provides that initarg in a direct slot definition
  (when (and (not pathname)
             (or (eq slot-names t)
                 (and slot-names
                      (find 'asdf::relative-pathname
                            (the cons slot-names)
                            :test #'eq))))
    ;; NB: Cannot (SETF COMPONENT-PATHNAME) [ASDF 3.1.3.8]
    ;; ... because there's no writer method for the slot
    ;;
    ;; Here, will set a default for the component's pathname,
    ;; using simply a "hack" onto the initargs.
    (setf (getf initargs :pathname)
	  (cond
            ;; FIXME PARENT may be derived in the next method
	    (parent (component-pathname parent))
            ;; FIXME by side effect, uses *DEFAULT-PATHNAME-DEFAULTS* in the
            ;; calling lexical environment. Should recurse upwards to use
            ;; the directory of the first non-nil component pathname of
            ;; the containing module environment, lastly to use the
            ;; directory of the system definition pathname
	    (t (make-pathname  :directory '(:relative))))))

  (when (next-method-p)
    ;; NB: Would only need to use this form if initargs was locally modified
    (apply #'call-next-method instance slot-names initargs))


  (macrolet ((init-default (arg value)
               (let ((%arg `(quote ,arg)))
                 `(when (or (eq slot-names t)
                            (and slot-names
                                 (member ,%arg (the cons slot-names)
                                         :test #'eq)))
                    (unless (slot-boundp instance ,%arg)
                      (setf (slot-value instance ,%arg)
                            ,value)))))
             (init-default-bm (slot-name type obj-name instance)
               (let ((%cbm-path (make-symbol "%cbm-path"))
                     (%cname (make-symbol "%cname")))
                 `(init-default ,slot-name
                      (let ((,%cname (format nil "~A-~A"
                                             (asdf:coerce-name ,obj-name)
                                             (asdf:coerce-name
                                              (quote ,slot-name))))
                            (,%cbm-path
                             (make-pathname :name ,obj-name
                                            :type ,type
                                            :defaults
                                            (component-pathname ,instance))))
                        ;; NB this defaults to storing a BITMAP not
                        ;; an ASDF COMPONENT into the slot.
                        ;;
                        ;; FIXME: This could be adapted to be of a
                        ;; smaller footprint -- namely, as to store a
                        ;; PATHNAME into the slot, instead of a complete
                        ;; BITMAP component instance -- considering:
                        ;; (A) The CURSOR is not a MODULE;  (B) Any
                        ;; COMPONENT stored in the slot might not be
                        ;; used as an ADSF component, except insofar as
                        ;; for storing a pathname
                        (make-instance 'bitmap ;; FIXME hard-coded class name
                                       :name ,%cname
                                       :pathname ,%cbm-path
                                       :parent ,instance))))))

    (let ((obj-name (component-name instance))
          (rpath (component-pathname instance)))
      (when obj-name
        ;; derive instance cursor and mask pathnames from component name
        ;; and component pathname
        (cond
          (rpath
           (init-default-bm cursor-bitmap "cursor" obj-name instance)
           (init-default-bm mask-bitmap "mask" obj-name instance))
          (t (flet ((check (slot)
                      (unless (slot-boundp instance slot)
                        (warn 'simple-style-warning
                              :format-control
                              "~<Unable to infer default value for ~S slot ~S~>~%
~<Component pathname not available for ~0@*~S~>"
                              :format-arguments (list instance slot)))))
               (check 'cursor-bitmap)
               (check 'mask-bitmap))))))
    (values instance)))



;;; BITMAP-PATHNAMES


(deftype image ()
  ;; cf. GEM::X-READ-AN-IMAGE
  #-:xlib t ;; ...
  #+:xlib
  '(or xlib:image-z xlib:image-xy xlib:image-x))



(defun bitmap-pathnames (name &key (errorp t)
                                    (system "garnet-bitmaps")
                                    ;; ^ NB kind-of arbitrary, specific
                                    ;; to prototyping for Garnet
                                    submodule)
  ;; NB/FIXME This function does not make any distinction for the
  ;; "Object State" of the given SYSTEM, such as for whether the
  ;; pathname should be resolved as onto the source filesystem -- i.e
  ;; the behavior essentially provided here -- or else resolved such as
  ;; onto XDG app-dirs, in an installed system.

  ;; Convenience function for integrating the local PIXMAP and CURSOR
  ;; classes into Garnet

  ;; FIXME: Should also generalize this for application e.g. onto the
  ;; Gilt system (bitmaps, XPM pixmaps) - garnet:lib;gilt;
  ;;
  ;; also gesture classifiers - garnet:lib;gesture;
  ;;
  ;; Therefore: TO DO
  ;;  1) [X] Move this into a (new) garnet-desktop-shared system [DONE]
  ;;  2) [X] Add a keyword arg :SYSTEM [DONE]
  ;;  3) Use for all of the systems:
  ;;       [X] garnet-bitmaps
  ;;       [X] garnet-pixmaps
  ;;       [X] garnet-c32-resources
  ;;       garnet-gesture-resources [TO DO] (*.classifier ?)
  ;;       garnet-gilt-resources [TO DO]
  ;;       ?? ../data/cirles.data used in demo-virtual-agg [TO DO]
  (declare (type string name)
           (type (or null string) submodule)
           (values (or pathname null)
                   (or pathname null)))
  (let* ((%name (asdf:coerce-name name))
         (sys (asdf:find-system system t))
         (c (cond
              (submodule
               (let* ((%submodule (asdf:coerce-name submodule))
                      (module (find-component sys %submodule)))

                 (asdf:find-component module %name)))
              (t (asdf:find-component sys %name)))))

    ;; (assert is-a-bitmap-resource c &fluff)

    (cond
      (c
       (typecase c
         ;; NB Static dispatch with TYPECASE
         (cursor
          (macrolet ((get-path (property obj)
                       ;; FIXME revise for storing a pathname such that
                       ;; the accessor may retrieve and when that's so,
                       ;; return that pathname, w/o further component
                       ;; accessor calls
                       `(asdf:component-pathname (,property ,obj))))
            (values
             (get-path cursor-cursor-bitmap c)
             (get-path cursor-mask-bitmap c))))
         (t (values (asdf:component-pathname c) nil))))
      (errorp
       ;; FIXME: use or extend ASDF:MISSING-COMPONENT
       ;; in which the missing component is named in the 'REQUIRES' slot NB
       (error 'simple-program-error
              :format-control
              "System ~A does not contain a bitmap named ~S"
              :format-arguments (list sys %name )))
      (t (values nil nil)))))


;;; Misc tests

;; NB: This may cause effective auto-loading of the garnet-bitmaps
;; system definition when the following forms are evaluated

;; (bitmap-pathnames "downarrow")
;; >  *.bm (pixmap) pathname, NIL

;; (bitmap-pathnames "line" :system "garnet-bitmaps" :submodule "garnetdraw")
;; >  *.bm (pixmap) pathname, NIL

;; (bitmap-pathnames "garnet")
;; > cursor, mask pathnames

;; (bitmap-pathnames "dnw")

;; (bitmap-pathnames "dnw" :system "also-dnw")



;; (asdf:module-components (asdf:find-component  "garnet-bitmaps" "garnet"))
;;
;; (describe (asdf:find-component  "garnet-bitmaps" "garnet"))
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
;; (enumerate-slot-values (asdf:find-component  "garnet-bitmaps" "garnet"))
;; ^ NB: CHILDREN and CHILDREN-BY-NAME slots were empty (NIL) in some versions of ASDF


;; (component-pathname (cursor-cursor-bitmap (asdf:find-component "garnet-bitmaps" "garnet" )))
