;; garnet-gadgets.asd				-*-lisp-*-
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2018 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Porting from garnet-loader.lisp onto ASDF
;;
;;------------------------------------------------------------------------------
;;
;; This system contains source code derived from source code that was
;; originally published under the following license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing list

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-sys)

(defsystem #:garnet-gadgets
  :version "1.0" ;; NB from gadgets-loader.lisp
  :desystem-depends-on #:garnet-shared
  :default-component-class garnet-source-file
  :serial nil
  :depends-on (#:garnet-shared #:garnet-aggregadgets)
  :components
  (;; NB: In this representation for ASDF, the #:GG package
   ;;     is defined in the granet-opal system

   ;; Commentary - Porting from Garnet-Loader and Subsq.
   ;;
   ;; The original Garnet Gadgets system uses numerous
   ;; <component>-loader files, those defining dependencies of each
   ;; <component>.
   ;;
   ;; The <component>-loader flies are each named in
   ;; ./gadgets-compiler.lisp within the top-level garnet-copy-files form,
   ;; in the source file
   ;;
   ;; Additionally, gadgets-loader.lisp may define some files
   ;; for which there's no explicit <component>-loader, namely under
   ;; GARNET-GADGETS-FILES

   ;; The dependencies, below, are derived from the respective
   ;; <foo>-loader files. The annotations in comments are derived mostly
   ;; from ./gadgets-compiler.lisp
   ;;
   ;; FIXME Double-check component linkage.

   (:file "prop-value-gadgets")
   ;; ^ FIXME: Depends on aggregadgets - schema opal:aggregadget
   ;
   ;; TO DO: consider moving prop-value-gadgets, and every component
   ;; depending on it, into the garnet-aggregadgets system
   (:file "prop-sheet"
          :depends-on ("error-gadget"
                       "prop-value-gadgets"))
   (:file "prop-sheet-win"
          :depends-on ("text-buttons"
                       "error-gadget"
                       "prop-value-gadgets"
                       "prop-sheet"))
   (:file "motif-prop-sheet-win"
          :depends-on ("motif-text-buttons"
                       "error-gadget"
                       "prop-value-gadgets"
                       "prop-sheet"))



   (:file "GAD-scroll-parts")    ;;  Helper modules containing definitions for
   (:file "GAD-slider-parts")    ;;    scroll bar and slider objects
   (:file "GAD-v-arrows")
   (:file "GAD-v-boxes")
   (:file "GAD-h-arrows")
   (:file "GAD-h-boxes")
   (:file "GAD-button-parts")    ;;  Helper module for button and menu objects

   (:file "h-scroll-bar"
          :depends-on ("GAD-scroll-parts"
                       "GAD-h-arrows"
                       "GAD-h-boxes"))
   (:file "browser-gadget"
          :depends-on ("h-scroll-bar" "scrolling-menu"))
   (:file "arrow-line")

   (:file "error-gadget-utils")
   (:file "error-gadget"
          :depends-on ("error-gadget-utils"
                       "text-buttons"))

   (:file "gauge")
   (:file "polyline-functions")
   (:file "polyline-creator"
          :depends-on ("polyline-functions"))

   (:file "h-slider"
          :depends-on ("GAD-scroll-parts"
                       "GAD-slider-parts"
                       "GAD-h-arrows"
                       "GAD-h-boxes"))
   (:file "labeled-box")
   (:file "menu")
   (:file "menubar-functions")
   (:file "menubar"
          :depends-on ("menubar-functions"))

   (:file "motif-parts"
	  :depends-on ("GAD-scroll-parts"))
   (:file "motif-text-buttons"
          :depends-on ("motif-parts"))
   (:file "motif-check-buttons"
          :depends-on ("motif-parts"))
   (:file "motif-error-gadget"
          :depends-on ("motif-text-buttons"
                       "error-gadget"))
   (:file "motif-gauge"
          :depends-on ("motif-parts"))
   (:file "motif-h-scroll-bar"
          :depends-on ("motif-parts"))
   (:file "motif-menu"
          :depends-on ("motif-parts"))
   (:file "motif-menubar"
          :depends-on ("motif-parts"
                       "motif-menu"
                       "menubar-functions"))
   (:file "motif-option-button"
          :depends-on ("motif-text-buttons"
                       "motif-menu"))
   (:file "motif-radio-buttons"
          :depends-on ("motif-parts"))
   (:file "motif-load-gadget"
	  :depends-on ("motif-scrolling-labeled-box"
		       "motif-scrolling-menu"
		       "save-load-functions"))
   (:file "motif-save-gadget"
          :depends-on ("motif-scrolling-labeled-box"
                       "motif-error-gadget"
                       "motif-scrolling-menu"
                       "save-load-functions"
                       "motif-load-gadget"))
   (:file "motif-scrolling-labeled-box"
          :depends-on ("scrolling-input-string"))
   (:file "motif-scrolling-menu"
          :depends-on ("motif-v-scroll-bar"))
   (:file "motif-scrolling-window"
          :depends-on ("motif-h-scroll-bar"
                       "motif-v-scroll-bar"
                       "scrolling-window-parts"))
   (:file "motif-slider"
          :depends-on ("motif-parts"
                       "motif-v-scroll-bar"))
   (:file "motif-trill-device"
          :depends-on ("motif-h-scroll-bar"))
   (:file "motif-v-scroll-bar"
          :depends-on ("motif-parts"))

   (:file "mouseline")
   (:file "multifont-gadget")
   (:file "option-button"
          :depends-on ("text-buttons"
                       "menu"))
   (:file "popup-menu-button"
          :depends-on ("menu"))


   (:file "radio-buttons"
          :depends-on ("GAD-button-parts"))

   (:file "save-load-functions")
   (:file "load-gadget"
	  :depends-on ("text-buttons"
		       "save-load-functions"
		       "scrolling-labeled-box"
		       "scrolling-menu"))
   (:file "save-gadget"
          :depends-on ("load-gadget" 
		       "error-gadget"))

   (:file "scrolling-input-string")
   (:file "scrolling-labeled-box"
          :depends-on ("scrolling-input-string"))
   (:file "scrolling-unlabeled-box"
          :depends-on ("scrolling-labeled-box"))
   (:file "scrolling-menu"
          :depends-on ("v-scroll-bar"))
   (:file "scrolling-window-parts")
   (:file "scrolling-window"
          :depends-on ("v-scroll-bar"
                       "h-scroll-bar"
                       "scrolling-window-parts"))

   (:file "graphics-selection") ;;  Selection squares for move-grow interaction
   (:file "multi-selection")

   (:file "standard-edit"
          :depends-on ("multi-selection"))
   (:file "text-buttons"
	  :depends-on ("GAD-button-parts"))
   (:file "trill-device"
          :depends-on ("GAD-scroll-parts"
                       "GAD-slider-parts"
                       "GAD-h-arrows"
                       "GAD-h-boxes"))
   (:file "v-scroll-bar"
          :depends-on ("GAD-scroll-parts"
                       "GAD-v-arrows"
                       "GAD-v-boxes"))
   (:file "v-slider"
          :depends-on ("GAD-scroll-parts"
                       "GAD-slider-parts"
                       "GAD-v-arrows"
                       "GAD-v-boxes"))
   (:file "x-buttons"
          :depends-on ("GAD-button-parts"))

   ))
