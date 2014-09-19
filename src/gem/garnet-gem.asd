;; garnet-gem.asd			-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:find-system '#:garnet-shared))

(in-package #:garnet-systems)

(defsystem #:garnet-gem
  :default-component-class garnet-source-file
  :serial t
  :depends-on (#:garnet-shared #:clx #:garnet-kr)
  ;; :perform (compile-op :before (o c)
  ;;           (let ((gem-p (find-package '#:gem)))
  ;;             ;; reset GEM::*DEVICE-INITIALIZERS*
  ;;             (when gem-p
  ;;               (let ((s (find-symbol
  ;;                         #.(symbol-name '#:*device-initializers*)
  ;;                         gem-p)))
  ;;                 (when (boundp s)
  ;;                   (set s nil))))))
  :components ((:file "package")
               (:file "gem")
               (:file "define-methods")
               (:file "x")
               ))
