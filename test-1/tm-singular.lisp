#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package #:tm)

(defun test-tm-singular-m0 ()
  (let*(
        (x (mk 'tm-singular :mount 3))
        (y (mk 'tm-list))
        (z (r x))
        )
    (w x 71)
    (∧
      (= z 3)
      (= (r x) 71)
      (s x (be ∅) (be t))
      (a x 22 (be t) (be ∅))
      (equal (tape x) [71 22]) ; tm-type 'tm-singular, not sure I'm happy with this behavior
      (= (d x y (be 1) (be 2) (be 3)) 2)
      (= (d x ∅ (be 1) (be 2) (be 3)) 2)
      )))
(test-hook test-tm-singular-m0)


(defun test-tm-singular-0 ()
  (let(
        (tm (mount {1 2 3}))
        )
    (∧
      (typep tm 'tm-list)

      (= (d tm) 2)
      (typep tm 'tm-list)

      (= (d tm) 3)
      (typep tm 'tm-singular)

      (d tm ∅ (be 'incorrect) (be t)) ; head is now on rightmost
      )))
(test-hook test-tm-singular-0)

(defun test-tm-singular-1 ()
  (let(
        (spill (mk 'tm-list))
        (tm (mount {1 2 3}))
        )
    (∧
      (typep tm 'tm-list)
      (typep spill 'tm-void)

      (= (d tm spill) 2)
      (typep tm 'tm-list)
      (typep spill 'tm-singular)

      (= (d tm spill) 3)
      (typep tm 'tm-singular)
      (typep spill 'tm-list)

      (d tm spill (be nil) (be t)) ; fails, head is on rightmost
      (equal (unmount spill) {2 3})
      )))
(test-hook test-tm-singular-1)

(defun test-tm-singular-2 ()
  (let(
        (tm (mk 'tm-list))
        )
    (∧
      (typep tm 'tm-void)
      (equal (tape tm) ∅)

      (a tm 1)
      (typep tm 'tm-parked-singular)
      (equal (tape tm) 1)

      (a tm 2)
      (typep tm 'tm-parked-tape)
      (equal (tape (tape tm)) {2 1})

      (a tm 3)
      (typep tm 'tm-parked-tape)
      (equal (tape (tape tm)) {3 2 1})

      (= (d tm) 3)
      (typep tm 'tm-parked-tape)
      (equal (tape (tape tm)) {2 1})

      (= (d tm) 2)
      (typep tm 'tm-parked-singular)
      (equal (tape tm) 1)

      (= (d tm) 1)
      (typep tm 'tm-void)
      (equal (tape tm) ∅)

      (d tm ∅ (be ∅) (be t)) ; head is now on rightmost
      )))
(test-hook test-tm-singular-2)

