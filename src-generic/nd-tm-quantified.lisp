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
  (defgeneric eas* (tm tm-fill &optional cont-ok cont-not-supported cont-no-alloc)
    (:documentation 
      "calls #'as repeatedly on a mk-entangled-with of tm filling with successive objects from tm-fill.
       tm will not be stepped.  tm-fill will be stepped.
       "
      ))

  (defmethod eas*
    (
      (tm0 tape-machine) 
      fill
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (let(
          (tm1 (mk-entangled-with tm0))
          )
      (as*-1 tm1 fill cont-ok cont-not-supported cont-no-alloc)
      ))

;;--------------------------------------------------------------------------------
;; repeated by count operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (defgeneric an (tm tm-fill count &optional cont-ok cont-rightmost cont-no-alloc)
    (:documentation 
      "Similar to calling #'a n times on a mk-entangled-with of tm."
      ))

  (defmethod an
    (
      (tm nd-tape-machine)
      fill
      (n integer)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (λ(tm1 n)(declare (ignore tm1 n)(be ∅))))
      (cont-no-alloc (λ(tm1 n)(declare (ignore tm1 n)(error 'alloc-fail))))
      )
    (let(
          (tm1 (mk-entangled-with tm))
          )
      (loop repeat n do
        (r fill
          (λ(object)
            (a tm1 object 
              (λ()(s fill
                    #'do-nothing
                    (λ()(return-from an (funcall cont-rightmost tm1 n)))
                    ))
              (λ()(return-from an (funcall cont-no-alloc tm1 n)))
              ))
          (λ()(return-from an (funcall cont-rightmost tm1 n)))
          ))))
          


