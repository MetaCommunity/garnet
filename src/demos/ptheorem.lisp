;; ptheorem.lisp - a simple usage example onto Math

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:ptheorem
    (:use #:kr #:cl)))

(in-package #:ptheorem)

;; Utility functions

(defun geom-sum (a b)
  (sqrt (+ (expt a 2)
           (expt b 2))))

(defun geom-diff (a c)
  (sqrt (- (expt c 2)
           (expt a 2))))

;; Interface onto KR

(create-instance 'triangle nil
   (:a 3)
   (:b 4)
   (:c 5))

(create-instance 'right-triangle triangle
 (:docstring "c^2 = a^2 + b^2")

 (:a (o-formula (geom-diff (gvl :b) (gvl :c))
                3))
 (:b (o-formula (geom-diff (gvl :a) (gvl :c))
                4))
 (:c (o-formula (geom-sum (gvl :a) (gvl :b))
                5)))

;; SHOULD call G-VALUE first, on the slot :a,
;; before S-VALUE on slot :a, given that
;; :a has a slot value derived from formula
(g-value right-triangle :a)
;; => 3.0
;; ^ that value is not derived from the slot's initform

(s-value right-triangle :a 4)
;; => 4, nil

(g-value right-triangle :a)
;; => 4

(g-value right-triangle :b)
;; => 3.9999998

(g-value right-triangle :c)
;; => 5.656854
;; ^ "Peculiar"

(s-value right-triangle :a 4.0d0)
(g-value right-triangle :b)
;; => 3.9999997615814133d0
(g-value right-triangle :c)
;; => 5.656854080904983d0

(g-value right-triangle :a)
;; => 4.0d0

(s-value right-triangle :a 4)
(s-value right-triangle :b 3)
(g-value right-triangle :a)
;; ^ may vary, depending on how many times s-value ... :a,:b is called (??)
(g-value right-triangle :b)
(g-value right-triangle :c)


;; To do: "Floating point accuracy" + utilities

;; To Do: Also model interior angles in RIGHT-TRIANGLE
;;        using the standard trigonometric formulae
