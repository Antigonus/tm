#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions derive the remainder of the tape-machine interface while using only the
primitives from tm-primitives.  

There is no functional need for a new tape machine implementation to specialize these
functions.  Still, some implementations will want to specialize these functions for
performance reasons.

Because these are built upon the primitives, they can only be tested against implementations
of the primitives.


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; moving data
;;  --move this to src-array

  ;; In repeated move operations we probably throw the displaced objects away if the
  ;; programmer wants to keep them xhe should copy them first, complications with
  ;; implementing this more efficiently on lists due to head cell locations with shared
  ;; tapes. In any case with repeated ops we can hop n places instead of shuffling.
  ;;
    (defgeneric m (tm fill)
      (:documentation
        "The object in rightmost is returned.
         All other objects on the tape move right one cell.
         Leftmost is written with the provided fill-object. 
         "
        ))

    (defmethod m 
      (
        (tm tape-machine)
        fill-object
        )
      (⟳ (λ(cont-loop cont-return)
           (let((displaced-object (r tm)))
             (w tm fill-object)
             (setf fill-object displaced-object)
             (s tm cont-loop cont-return)
             )))
      fill-object
      )

