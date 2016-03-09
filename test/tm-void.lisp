#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package #:tm)


;;--------------------------------------------------------------------------------
;; tm-void
;;
  (defun test-heads-on-same-cell-void-0 ()
    (let(
          (a (mk-tm-void))
          (b (mk-tm-void))
          (c (make-instance 'tape-machine))
          )
      (setf (HA c) 1)
      (setf (tape c) 2)
      (∧
        (heads-on-same-cell a b)
        (¬ (heads-on-same-cell a c))
        (¬ (heads-on-same-cell c a))
        )))
   (test-hook test-heads-on-same-cell-void-0)

