;; package.lisp -- package definitions for gem, opal, interactors systems
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2014-2018 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Porting from garnet-loader.lisp
;;
;;------------------------------------------------------------------------------
;;
;; This file contains source code derived from source code that was
;; originally published in garnet-loader.lisp under the following
;; license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing list.
;;
;; Those top-level forms are denoted, below, as derived from
;; garnet-loader.lisp

;; NB: Along with the GEM package, The OPAL and INTER packages are also
;; defined here, in an effort to alleviate concerns with regards to
;; forward references when compiling or otherwise evaluating source code
;; in the Garnet system [spchamp]

(in-package #:cl-user)

(defpackage #:gem
  ;; derived from garnet-loader.lisp
  (:use #:common-lisp
	#:kr #:kr-debug)
  (:export
   #:active-devices
   #:init-device

   ;; referenced in gem x.lisp
   #:*black*
   #:set-draw-function-alist
   #:set-window-property
   #+clx #:x-set-window-property

   ;; referenced in opal defs.lisp
   ;;
   ;; NB: Do these shadow anything?
   #:display-info
   #:make-display-info
   #:copy-display-info
   #:Display-Info
   #:Make-Display-Info #:Copy-Display-Info
   #:Display-Info-Display #:Display-Info-Screen #:Display-Info-Root-Window
   #:Display-Info-Line-Style-GC #:Display-Info-Filling-Style-GC
   #:*update-lock* #:*screen-width* #:*screen-height*
   #:*Fixed-Font-Family* #:*Serif-Font-Family* #:*Sans-Serif-Font-Family*
   #:*Small-Font-Size* #:*Medium-Font-Size*
   #:*Large-Font-Size* #:*Very-Large-Font-Size*
   #:*Small-Font-Point-Size* #:*Medium-Font-Point-Size*
   #:*Large-Font-Point-Size* #:*Very-Large-Font-Point-Size*
   #:default-font-from-file
   ;; ... same file ...
   #:*color-screen-p*

   ;; referenced in opal basics.lisp
   #:*function-alist*

   ;; reference anywhere (may contain duplicates)
   #:*exposure-event-mask
   #:*Fixed-Font-Family*
   #:*function-alist
   #:*Large-Font-Point-Size*
   #:*Large-Font-Size*
   #:*Small-Font-Point-Size*
   #:*Small-Font-Size*
   #:*update-lock*
   #:active-devices
   #:all-garnet-windows
   #:beep
   #:bit-blit
   #:black-white-pixel
   #:character-width
   #:Check-Double-Press
   #:clear-area
   #:clear-area.
   #:color-to-index
   #:colormap-property
   #:copy-to-pixmap
   #:create-cursor
   #:create-image
   #:create-image-array
   #:create-pixmap
   #:create-state-mask
   #:create-window
   #:default-font-from-file
   #:delete-font
   #:delete-pixmap
   #:delete-window
   #:device-image
   #:Device-Image
   #:discard-mouse-moved-events
   #:discard-pending-events
   #:Display-Info
   #:DISPLAY-INFO
   #:display-info-display
   #:Display-Info-Display
   #:display-info-filling-style-gc
   #:display-info-line-style-gc
   #:Display-Info-Line-Style-GC
   #:draw-arc
   #:draw-image
   #:draw-line
   #:draw-lines
   #:draw-points
   #:draw-rectangle
   #:draw-roundtangle
   #:draw-text
   #:drawable-to-window
   #:event-handler
   #:flush-output
   #:font-exists-p
   #:font-max-min-width
   #:font-name-p
   #:font-to-internal
   #:gdi-set-window-property
   #:get-cut-buffer
   #:image-bit
   #:image-from-bits
   #:image-hot-spot
   #:image-size
   #:image-to-array
   #:init-device
   #:initialize-device
   #:initialize-window-borders
   #:inject-event
   #:MAC-set-window-property
   #:Make-Display-Info
   #:make-font-name
   #:Make-Font-Name
   #:map-and-wait
   #:max-character-ascent
   #:max-character-descent
   #:mouse-grab
   #:raise-or-lower
   #:read-an-image
   #:reparent
   #:set-clip-mask
   #:set-cut-buffer
   #:set-device-variables
   #:Set-Device-Variables
   #:set-draw-function-alist
   #:set-drawable-to-window
   #:set-interest-in-moved
   #:set-window-property
   #:text-extents
   #:Text-Extents
   #:text-width
   #:translate-character
   #:translate-code
   #:translate-coordinates
   #:translate-mouse-character
   #:window-debug-id
   #:window-depth
   #:window-from-drawable
   #:window-has-grown
   #:window-to-image
   #:write-an-image
   #:x-set-window-property

  ))

(defpackage #:opal
  ;; derived from garnet-loader.lisp
  (:use #:common-lisp
	#:kr)
  (:export #:black #:white ;; referfenced in x.lisp / Garnet 3.3 (??)
           ))

(defpackage #:interactors
  ;; derived from garnet-loader.lisp
  (:use #:common-lisp #:kr)
  (:nicknames #:inter)
  (:export
   #:*garnet-break-key*
   #:*left-button*
   #:*trans-from-file*))
