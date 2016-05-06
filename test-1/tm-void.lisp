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
  (defun test-void-0 ()
    (let(
          (x (mk 'tm-void))
          (y (mk 'tm-list))
          )
      (∧
        (s x (be ∅) (be t))
        (a x 22 (be ∅) (be t)) ; this fails because the base type is 'tm-void
        (= (d x y (be 1) (be 2) (be 3)) 2)
        (= (d x ∅ (be 1) (be 2) (be 3)) 2)
        )))
  (test-hook test-void-0)


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
           (tm1  (mk 'tm-void :tm-type 'tm-list))
          )
      (∧
        (= flag 2)
        (eq (type-of tm1) 'tm-void)

        (a tm1 7)
        (eq (type-of tm1) 'tm-parked)

        (s tm1)
        (eq (type-of tm1) 'tm-list)
        (= (r tm1) 7)
        (¬ (s tm1))

        (a tm1 14)
        (eq (type-of tm1) 'tm-list)
        (= (r tm1) 7)
        (s tm1)
        (= (r tm1) 14)
        (¬ (s tm1))
        )))
  (test-hook test-void-1)

 (defun test-void-2 ()
    (let*(
           (vi  (make-instance 'tm-void))
           (flag (init vi {:mount {1 2 3}} (be 1) (be 2)))
           (tm1  (mk 'tm-void :tm-type 'tm-list))
          )
      (∧
        (= flag 2)
        (typep tm1 'tm-void)

        (a tm1 7)
        (typep tm1 'tm-parked)

        (a tm1 14)
        (typep tm1 'tm-parked)

        (s tm1)
        (typep tm1 'tm-list)
        (= (r tm1) 14)
        (s tm1)
        (= (r tm1) 7)
        (¬ (s tm1))
        )))
  (test-hook test-void-2)
;;;(T TM-VOID T TM-PARKED-TAPE T TM-PARKED-TAPE T TM-LIST 7 T 14 T)
