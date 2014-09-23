;; garnet-debug.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-debug
  :version "2.0"
  :default-component-class garnet-source-file
  ;; Specifically this system depends on the following gadgets:
  ;;
  ;;   multifont-loader
  ;;   text-buttons
  ;;   error-gadget-utils
  ;;
  ;; see also: debug-loader.lisp
  :serial t
  :version "2.0"
  :depends-on (#:garnet-shared #:garnet-gadgets
			       #:garnet-bitmaps
			       #:garnet-pixmaps)
  :components ((:file "debug-fns")
	       (:file "objsize")
	       (:file "inspector")
	       (:file "suggest-constants")
	       ))

;; FIXME: 
;; 1. Must also define systems for:
;; .../../lib/bitmaps/
;; .../../lib/pixmaps/
;;
;; 2. then ensure that the following systems depend on
;;    
;;  A. Depending on garnet-bitmaps :
;;       garnet-demos
;;       garnet-gadgets (various specific gadgets)
;;       garnet-opal
;;       garnet-lapidary
;;       garnet-debug (this system)
;;  B. Depending on garnet-pixmaps :
;;       garnet-demos
;;       garnet-gilt
;;       garnet-debug (this system)

(defvar Garnet-Pixmap-Pathname
  (append-directory Garnet-Lib-Pathname "pixmaps"))

;; FIXME: Also include the following variable definitions
;; in the respective system definitions

(defvar Garnet-Gilt-Bitmap-Pathname ;; gilt
  (append-directory Garnet-Lib-Pathname "gilt"))

(defvar Garnet-C32-Bitmap-Pathname ;; c32
  (append-directory Garnet-Lib-Pathname "c32"))

(defvar Garnet-DataFile-Pathname ;; garnet-demos
  (append-directory Garnet-Lib-Pathname "data"))

(defvar Garnet-Gesture-Data-Pathname ;; garnet-gesture
  (append-directory Garnet-Lib-Pathname "gesture"))

