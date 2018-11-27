;; garnet-gem.asd				-*-lisp-*-
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
;; This system contains source code originally published in garnet-loader.lisp
;; under the following license stipulations:
;;
;;   This code was written as part of the Garnet project at
;;   Carnegie Mellon University, and has been placed in the public
;;   domain.  If you are using this code or any part of Garnet,
;;   please contact garnet@cs.cmu.edu to be put on the mailing list.

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require '#:asdf)
  (asdf:find-system '#:garnet-shared)
  )

(in-package #:garnet-sys)

(defsystem #:garnet-gem
  :version "1.0" ;; NB from gem-loader.lisp
  :serial t
  :default-component-class garnet-source-file
  :defsystem-depends-on (#:garnet-shared)
  :depends-on (#-(and apple (not clx)) #:clx ;; ...
               #:garnet-kr)
  :perform (load-op :after (o c)
                    ;; FIXME: define an INIT-OP and move this to there
	       (with-functions ((register #:ensure-device-initializer
                                          ;; ^ new in gem.lisp [spchamp]
					  #:gem)
                                #+(and apple (not clx))
                                (mactli #:mac-top-level-initialize
                                        ;; cf. ./mac.lisp
                                        #:gem)
                                #-(and apple (not clx))
				(xtli #:x-top-level-initialize
                                      ;; cf. ./x.lisp
				      #:gem))
                 ;; NB: see also, the garnet-opal sysdef, such that
                 ;; provides further initialization within a PERFORM
                 ;; method -- principally, utilizing the respective
                 ;; platform device initializer such that will have been
                 ;; defined here in the Gem system 
                 #-(and apple (not clx))
                 (funcall register :mac mactli) ;; cf ./mac.lisp
                 #-(and apple (not clx))
	         (funcall register :x xtli) ;; cf ./x.lisp
                 ))
  :components ((:file "package")
               (:file "gem")
               (:file "define-methods")
               ;; NB: The following were referenced originally onto opal-loader.lisp
               #-(and apple (not clx)) (:file "x")
               ;; similarly, mac.lisp
               #+(and apple (not clx)) (:file "mac")
               ))
