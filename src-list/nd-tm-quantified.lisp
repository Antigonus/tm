#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Quantification

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; indexed read and write
;;
  (defgeneric esnr 
    (
      tm
      index
      &optional
      cont-ok
      cont-rightmost
      )
    (:documentation
      " This is an indexed read operation.  It makes an entangled copy of 'tm', 
        steps it 'index' times, then reads it.
      "
      ))

  (defmethod esnr 
    (
      tm
      index
      &optional
      (cont-ok #'echo)
      (cont-rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
      )
    (with-mk-entangled tm
      (λ(tm1)
        (sn tm1 index
          (λ()(funcall cont-ok (r tm1)))
          (λ(n)(funcall cont-rightmost n))
          ))))

  (defgeneric esnw 
    (
      tm
      index
      instance
      &optional
      cont-ok
      cont-rightmost
      )
    (:documentation
      " This is an indexed write operation.  It makes an entangled copy of 'tm', 
        steps it 'index' places, then writes the 'instance'.
      "
      ))

  (defmethod esnw 
    (
      tm
      index
      instance
      &optional
      (cont-ok (be t))
      (cont-rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
      )
    (with-mk-entangled tm
      (λ(tm1)
        (sn tm1 index
          (λ()(w tm1 instance cont-ok #'cant-happen))
          (λ(n)(funcall cont-rightmost n))
          ))))

;;--------------------------------------------------------------------------------
;; repeated until end of tape operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (defgeneric eas* (tm tm-fill &optional cont-ok cont-no-alloc)
    (:documentation 
      "calls #'as repeatedly on a mk-entangled copy of tm, filling
       with successive instances from tm-fill.
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

        
          


