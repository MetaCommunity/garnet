;; garnet-pixmaps.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared)

  (dolist (s '(#:garnet-desktop-shared))
    (asdf:operate 'asdf:load-op s)))


(in-package #:garnet-systems)

(defsystem #:garnet-pixmaps

  :class resource-system
  :serial t
  :defsystem-depends-on (#:garnet-desktop-shared
                         #:garnet-shared)

  :description "System for Garnet pixmaps"

  ;; FIXME Document the pixmap compoment class
  ;; FIXME Integrate this w/ Garnet runtime code
  ;;        - NB ASDF pathname translations
  :components
  ((pixmap "arrdown")
   (pixmap "arrleft")
   (pixmap "arrne")
   (pixmap "arrright")
   (pixmap "arrup")
   (pixmap "bee")
   (pixmap "block")
   (pixmap "bomb")
   (pixmap "bull")
   (pixmap "circles")
   (pixmap "city")
   (pixmap "crab45")
   (pixmap "crab")
   (pixmap "doso")
   (pixmap "doss")
   (pixmap "dos")
   (pixmap "editor")
   (pixmap "escherknot")
   (pixmap "eye10")
   (pixmap "eye11")
   (pixmap "eye12")
   (pixmap "eye13")
   (pixmap "eye14")
   (pixmap "eye15")
   (pixmap "eye16")
   (pixmap "eye17")
   (pixmap "eye18")
   (pixmap "eye19")
   (pixmap "eye1")
   (pixmap "eye20")
   (pixmap "eye21")
   (pixmap "eye22")
   (pixmap "eye23")
   (pixmap "eye24")
   (pixmap "eye2")
   (pixmap "eye3")
   (pixmap "eye4")
   (pixmap "eye5")
   (pixmap "eye6")
   (pixmap "eye7")
   (pixmap "eye8")
   (pixmap "eye9")
   (pixmap "eye")
   (pixmap "fils")
   (pixmap "fil")
   (pixmap "floppy")
   (pixmap "garnetlogo")
   (pixmap "jolifond")
   (pixmap "koala")
   (pixmap "lambda")
   (pixmap "martini")
   (pixmap "mickey")
   (pixmap "mouse")
   (Pixmap "NeXT")
   (pixmap "nobozos")
   (pixmap "noseback")
   (pixmap "nosefront")
   (pixmap "nose")
   (pixmap "nosmoking")
   (Pixmap "Plaid")
   (pixmap "porsche")
   (pixmap "stopsign")
   (pixmap "test2")
   (pixmap "test3")
   (pixmap "umbrella")
   (pixmap "wave2")
   (pixmap "wingdogs")
   ))

;; example application:
;; (find-bitmap-pathname "NeXT" :system "garnet-pixmaps")
