;; sys-utils.lisp - Reusable Utilities for Garnet System Definitions
;;------------------------------------------------------------------------------
;;
;; Copyright (c) 2018 Sean Champ and others. All rights reserved.
;;
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this distribution
;; and is available at http://www.eclipse.org/legal/epl-v10.html
;;
;; Contributors: Sean Champ - Initial API and implementation
;;
;;------------------------------------------------------------------------------

(in-package #:garnet-sys)

(defmacro with-functions (specs &body body)
  ;; NB defined as an abbreviation of LTP-UTILS:WITH-SAFE-FREFS
  ;;
  ;; This macro provides a portable methodology for making forward
  ;; reference to function definitions, for named functions and symbol
  ;; packages as may not yet be defined at the time when the
  ;; macroexpansion is evaluated.
  ;;
  ;; This macro may be applied within the method bodies of DEFSYSTEM
  ;; :PERFORM methods, such as for any system definition providing a
  ;; definition of a function as may be referenced within a :PERFORM
  ;; method for the defining system. In a simple sense, it may serve to
  ;; simplify the sytnax for such forward references.
  ;;
  ;; An example is provided in the definitions of each of the
  ;; garnet-opal and
  (let ((%sym (make-symbol "%symbol"))
        (%foundp (make-symbol "%foundp")))
    (labels ((transform-ref (spec)
               (destructuring-bind (name fsym &optional
                                         (pksym nil pksymp)) spec
                 (declare (type symbol name fsym pksym))
                 (list name
                     `(multiple-value-bind (,%sym ,%foundp)
                          (find-symbol (symbol-name (quote ,fsym))
                                       ,@(when pksymp
                                           `((quote ,pksym))))
                        (cond
                          (,%foundp (fdefinition ,%sym))
                          (t (error "No symbol ~A found~<<in package ~A>>"
                                    (quote ,fsym) (quote ,pksym)))))))))

      (let ((%bind (mapcar #'transform-ref specs))
            (%names (mapcar #'car specs)))
        `(let (,@%bind)
           (declare (type function ,@%names))
           ,@body
           )))))


#+NIL ;; TBD - e.g cf :EAGER in garnet-kr
(defun op-with-features (features op component)
  (declare (type list features))
  (let ((*features* (append features *features*)))
    ;; NB: This is really rudimentary, and does not test what features the
    ;; component was previously evaluated under, if previously evalated
    ;; for an operation of the same class as OP
    ;;
    ;; NB: As this would lexically shadow CL:*FEATURES* then any changes
    ;; made onto CL:*FEATURES* within the context of this OP would not
    ;; be reflected in the global environment, without further
    ;; reflection onto CL:*FEATURES* as at the time when this OP is
    ;; performed, and subsequent to the completion of the OP whether
    ;; with "Normal" or "Non-normal" exit. Under "Normal" exit, any
    ;; changes onto *FEATURES* should be noted with a warning condition
    ;; ... unless set in something like an 'adds-features' slot on the
    ;; COMPONENT 
    (asdf:operate op component)))
