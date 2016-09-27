#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Quantification

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; repeated until end of tape operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (defgeneric eas* (tm tm-fill &optional cont-ok cont-no-alloc)
    (:documentation 
      "calls #'as repeatedly on a mk-entangled copy of tm, filling
       with successive objects from tm-fill.
       tm will not be stepped.  tm-fill will be stepped.
       "
      ))

  (defmethod eas*
    (
      (tm0 tape-machine) 
      fill
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (with-mk-entangled tm0
      (λ(tm1)
        (as* tm1 fill cont-ok cont-no-alloc)
        )))

;;--------------------------------------------------------------------------------
;; repeated by count operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (defgeneric an (tm count tm-fill &optional cont-ok cont-rightmost cont-no-alloc)
    (:documentation 
      "Similar to calling #'a n times on a mk-entangled-with of tm."
      ))

  (defmethod an
    (
      (tm nd-tape-machine)
      (n integer)
      fill
      &optional 
      (cont-ok (be t))
      (cont-rightmost (λ(tm1 n)(declare (ignore tm1 n))(be ∅)))
      (cont-no-alloc (λ(tm1 n)(declare (ignore tm1 n))(error 'alloc-fail)))
      )
    (with-mk-entangled tm
      (λ(tm1) (asn tm1 n fill cont-ok cont-rightmost cont-no-alloc))
      ))

        
          


