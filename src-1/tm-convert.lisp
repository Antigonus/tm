#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make machines from other objects.
  Make other objects from machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; converts a tape machine to another form
;;
  (defgeneric to-sequence(tm)) ; picks the easiest conversion to make

  (defgeneric to-list (tm))
  (defgeneric to-array-adj (tm))
  (defgeneric to-array (tm))

  ;; generic list maker, some specializations, particularly the tm-list specialization,
  ;; will be more efficient.
  (defmethod to-list ((tm0 tape-machine))
    (let(
          (tm1 (tm-mk 'tm-list))
          )
      (⟳ (λ(cont-loop cont-return) (as tm1 (r tm0) cont-loop cont-return)))
      (cdr (tape tm1))
      ))
