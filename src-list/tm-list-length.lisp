#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  machine is not void, so it will have a length of at least 1
  
|#
(in-package #:tm)

  (defun tm-list-singular (tm0)
    (¬ (cdr (tape tm0)))
    )

  (defmethod singular-0 ((tm0 tm-list) (state active) cont-true cont-false)
    (if (¬ (cdr (tape tm0)))
      (funcall cont-true)
      (funcall cont-false)
      ))
  (defmethod singular-0 ((tm0 tm-list) (state parked) cont-true cont-false)
    (if (¬ (cdr (tape tm0)))
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod doubleton-0 ((tm0 tm-list) (state active) cont-true cont-false)
    (if
      (∧
        (cdr (tape tm0))
        (¬ (cddr (tape tm0)))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))
  (defmethod doubleton-0 ((tm0 tm-list) (state parked) cont-true cont-false)
    (if
      (∧
        (cdr (tape tm0))
        (¬ (cddr (tape tm0)))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))
  
  (defmethod tripleton-0 ((tm0 tm-list) (state active) cont-true cont-false)
    (if
      (∧
        (cdr (tape tm0))
        (cddr (tape tm0))
        (¬ (cdddr (tape tm0)))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))
  (defmethod tripleton-0 ((tm0 tm-list) (state parked) cont-true cont-false)
    (if
      (∧
        (cdr (tape tm0))
        (cddr (tape tm0))
        (¬ (cdddr (tape tm0)))
        )
      (funcall cont-true)
      (funcall cont-false)
      ))
