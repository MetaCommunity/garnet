;; garnet-bitmaps.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared)

  (dolist (s '(#:info.metacommunity.cltl.utils
	       #:garnet-desktop-shared))
    (asdf:operate 'asdf:load-op s)))


(in-package #:garnet-systems)

(defsystem #:garnet-bitmaps

  :class resource-system
  :serial t
  :defsystem-depends-on (#:info.metacommunity.cltl.utils
			 #:garnet-desktop-shared
                         #:garnet-shared)
  :description "System for Garnet bitmaps and cursor resource files"

  :components
  ((bitmap "downarrow")
   (bitmap "indicator")
   (bitmap "pop-up-icon-no-border")
   (bitmap "pop-up-icon")

   (cursor "garbage")
   (cursor "garnet")
   (cursor "hourglass")
   (cursor "lapidary-copy")
   (cursor "lapidary-delete")
   (cursor "lapidary-instance")
   (cursor "lapidary-load")
   (cursor "lapidary-move")
   ;; NB: File "./jagged"
   ;; * No references found, within Garnet source tree
   ;; * File is in a bitmap format
   ;; * No accompanying <name>.mask file
   ;; * Not included in system definition
   (resource-module "garnetdraw"
                    :components
                    ((bitmap "doublelinearrow")
                     (bitmap "linearrow")
                     (bitmap "line")
                     (bitmap "oval")
                     (bitmap "polygon")
                     (bitmap "rectangle")
                     (bitmap "roundrect")))))
