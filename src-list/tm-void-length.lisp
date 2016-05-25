#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

  (defmethod empty ((tm0 tm-void) &optional (cont-true (be t)) (cont-false (be ∅)))
    (declare (ignore cont-false))
    (funcall cont-true)
    )
  (defmethod singleton ((tm0 tape-void) &optional (cont-true (be t)) (cont-false (be ∅)))
    (declare (ignore cont-true))
    (funcall cont-false)
    )

  (defmethod doubleton ((tm0 tape-void) &optional (cont-true (be t)) (cont-false (be ∅)))
    (declare (ignore cont-true))
    (funcall cont-false)
    )

  (defmethod tripleton ((tm0 tape-void) &optional (cont-true (be t)) (cont-false (be ∅)))
    (declare (ignore cont-true))
    (funcall cont-false)
    )

  (defmethod length-cmp
    (
      (tm0 tm-void)
      (n integer)
      &optional 
      (cont-longer (be 'longer))
      (cont-same   (be 'same))
      (cont-shorter (be 'shorter))
      )
    (when (< n 0) (return-from length-cmp (funcall cont-longer)))
    (if (= n 0) 
      (funcall cont-same)
      (funcall cont-shorter)
      ))

  ;; (length tm0) <?> (length tm1)
  (defmethod length-cmp
    (
      (tma tm-void)
      (tmb tm-void)
      &optional 
      (cont-longer (be 'longer))
      (cont-same   (be 'same))
      (cont-shorter (be 'shorter))
      )
    (declare (ignore cont-longer cont-shorter))
    (funcall cont-same)
    )
  (defmethod length-cmp
    (
      (tma tm-void)
      (tmb tape-machine)
      &optional 
      (cont-longer (be 'longer))
      (cont-same   (be 'same))
      (cont-shorter (be 'shorter))
      )
    (declare (ignore cont-same cont-longer))
    (funcall cont-shorter)
    )
  (defmethod length-cmp
    (
      (tma tape-machine)
      (tmb tm-void)
      &optional 
      (cont-longer (be 'longer))
      (cont-same   (be 'same))
      (cont-shorter (be 'shorter))
      )
    (declare (ignore cont-same cont-shorter))
    (funcall cont-longer)
    )

