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
         (a (mk 'tm-void))
         (b (mk 'tm-void))
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


 (defun test-void-1 ()
    (let*(
           (vi  (make-instance 'tm-void))
           (flag (init vi {:mount {1 2 3}} (be 1) (be 2)))
           (tm1  (mk 'tm-void :tape-space 'tm-list))
          )
      (∧
        (= flag 2)
        (eq (type-of tm1) 'tm-void)
        (a tm1 7)
        (eq (type-of tm1) 'tm-parked-singular)
        (s tm1)
        (eq (type-of tm1) 'tm-singular)
        (= (r tm1) 7)
        (¬ (s tm1))
        )))
  (test-hook test-void-1)
