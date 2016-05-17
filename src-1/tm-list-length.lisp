#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  machine is not void, so it will have a length of at least 1
  
|#
(in-package #:tm)

  (defun tm-list-singleton (tm0)
    (¬ (cdr (tape tm0)))
    )

  (defmethod singleton ((tm0 tm-list) &optional (cont-true (be t)) (cont-false (be ∅)))
    (if (¬ (cdr (tape tm0)))
      cont-true
      cont-false
      ))

  (defmethod doubleton ((tm0 tm-list) &optional (cont-true (be t)) (cont-false (be ∅)))
    (if
      (∧
        (cdr (tape tm0))
        (¬ (cddr (tape tm0)))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))
  
  (defmethod tripleton ((tm0 tm-list) &optional (cont-true (be t)) (cont-false (be ∅)))
    (if
      (∧
        (cdr (tape tm0))
        (cddr (tape tm0))
        (¬ (cdddr (tape tm0)))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))
