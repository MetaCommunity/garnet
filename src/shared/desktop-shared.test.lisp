;;; FIXME remove this file

(in-package #:garnet-syss)

(progn

  ;; setup form (dep. ASDF internals)
  (remhash "fooooooo" asdf::*defined-systems*)
  ;; FIXME: add trivial UNREGSTER-SYSTEM to ASDF

  ;; NOTE: This test form should be read
  ;; with *DEFAULT-PATHNAME-DEFAULTS*
  ;; denoting the directory containing this file

  (let* ((s (defsystem "fooooooo"
              ;; NOTE: This is part of the test's setup form
              :pathname #.*default-pathname-defaults*
              :components ((bitmap "downarrow")
                           (cursor "garnet"))))
         ;; test form follows
         (c (module-components s))
         (cursor (cadr c)))
    (values

     ;; cursor component-children test (informative)
     ;; (module-components cursor)

     ;; cursor pathname test
     (string= (namestring (component-pathname s))
              (namestring (component-pathname cursor)))

     ;; cursor child elements: pathname test
     (every #'(lambda (reslt)
                (pathnamep reslt))
            (mapcar #'probe-file
                    (mapcar #'component-pathname (module-components cursor))))
     (mapcar #'component-pathname (module-components cursor))

     ;; cursor child elements: component-path test
     (equal
      (mapcar #'component-find-path
	      (module-components cursor))
      '(("fooooooo" "garnet" "garnet-cursor-bitmap")
        ("fooooooo" "garnet" "garnet-mask-bitmap")))

     ;; bitmap component-path test (informative)
     (equal
      (component-find-path (car c))
      '("fooooooo" "downarrow"))

     ;; bitmap pathname test
     ;; (component-pathname (car c)) ;; informative

     (when (probe-file (component-pathname (car c)))
       (values t))

     ;; a key test, onto an existing system with valid pathnames defined:
     (pathnamep (probe-file (component-pathname (cursor-cursor-bitmap (find-component* "garnet" "garnet-bitmaps")))))
     )))



