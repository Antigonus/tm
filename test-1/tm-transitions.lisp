#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package #:tm)

(defun test-tm-singular-0 ()
  (let(
        (tm (mount {1 2 3}))
        )
    (∧
      (typep tm 'tm-list)

      (= (d◧ tm) 1)
      (typep tm 'tm-list)

      (= (d tm) 3)
      (typep tm 'tm-list)

      (d tm ∅ (be ∅) (be t)) ; head is now on rightmost

      (= (d◧ tm) 2)
      (typep tm 'tm-void)

      (d tm ∅ (be ∅) (be t)) ;can't dealloc from void (watch me pull a rabbit from my hat..)
      (d◧ tm ∅ (be ∅) (be t))
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
      (typep spill 'tm-parked-tape)

      (= (d tm spill) 3)
      (typep tm 'tm-tape)
      (typep spill 'tm-parked-tape)

      (d tm spill (be nil) (be t)) ; fails, head is on rightmost
      (equal (unmount spill) {2 3})

      (= (d◧ tm spill) 1)
      (typep tm 'tm-void)
      (typep spill 'tm-parked-tape)
      (equal (unmount spill) {1 2 3})

      (s spill)
      (typep spill 'tm-list)
      (= (r spill) 1)

      (= (d◧ spill) 1)
      (typep spill 'tm-void)
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
      (typep tm 'tm-parked-tape)
      (equal (tape tm) {1})

      (a tm 2)
      (typep tm 'tm-parked-tape)
      (equal (tape (tape tm)) {2 1})

      (s tm)
      (a tm 3)
      (typep tm 'tm-list)
      (equal (tape (tape tm)) {2 3 1})

      (= (d tm) 3)
      (typep tm 'tm-list)
      (equal (tape (tape tm)) {2 1})

      (park tm)
      (= (d tm) 2)
      (typep tm 'tm-parked-list)
      (equal (tape tm) 1)

      (= (d tm) 1)
      (typep tm 'tm-void)
      (equal (tape tm) ∅)

      (d tm ∅ (be ∅) (be t)) 
      (d◧ tm ∅ (be ∅) (be t)) 
      )))
(test-hook test-tm-singular-2)

