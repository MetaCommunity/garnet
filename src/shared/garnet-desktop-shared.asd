;; garnet-desktop-shared.asd			-*-lisp-*-
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

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:asdf)
  (asdf:find-system '#:garnet-shared)
  )

(in-package #:garnet-sys)


;; FIXME Document this system definition
;; See also garnet-shared.asd

;; NB: Consider renaming this system definition to reflect its function
;; as providing ASDF component classes for system definitions onto
;; Garnet. => garnet-desktop-sys

(defsystem #:garnet-desktop-shared
  :defsystem-depends-on (#:garnet-shared)
  :default-component-class garnet-source-file
  :documentation "System Components for Garnet Desktop Applications"
  :serial t
  :version "1.0"
  ;; :depends-on (#:clx) ;; only cf. deftype image
  ;;
  ;; FIXME consider moving this into the garnet-gem sysdef
  :components
  ((:file "desktop-shared")
   ))
