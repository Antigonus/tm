#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-list-tm-0 ()
  (let*(
         (tm0 (mk 'list-tm {:mount {1}}))
         (tm1 (mount {7 2 -3}))
         (tm2 (mk (type-of tm0) {:mount {2 3}}))
         )
    (∧
      (on-leftmost tm0)
      (on-leftmost tm1)
      (on-leftmost tm2)

      (typep tm0 'list-tm)
      (typep tm1 'list-tm)
      (typep tm2 'list-tm)

      (typep (tape tm1) 'cons)
      )))
(test-hook test-list-tm-0)

(defun test-list-tm-1 ()
  (let*(
         (tm0 (mk 'list-tm {:mount {1}}))
         (tm1 (make-instance 'list-tm))
         (tm2 (mk (type-of tm0) {:mount {2 3}}))
         )
    (∧
      (init tm1 {:mount {7 2 -3}} (be t) (be ∅))
      (init tm1 {:mount 7} (be ∅) (be t))
      (init tm1 {:a-parm 7} (be ∅) (be t))

      (= (r tm0) 1)
      (= (r tm1) 7)
      (= (r tm2) 2)

      (= (esr tm0 (be 1) (be 2)) 2)
      (= (esw tm0 3 (be 1) (be 2)) 2)

      (= (esr tm1) 2)
      (esw tm1 12)
      (= (esr tm1) 12)
      )))
(test-hook test-list-tm-1)

(defun test-list-tm-2 ()
  (let*(
         (tm0 (mk 'list-tm {:mount {1}}))
         (tm1 (mount {7 2 -3}))
         (tm2 (mk (type-of tm0) {:mount {2 3}}))
         )
    (∧
      (on-rightmost tm0)
      (= (s tm0 (be 7) (be 8)) 8)
      (s tm1)
      (¬ (on-rightmost tm1))
      (s tm2)
      (on-rightmost tm2)
      (= (s tm2 (be 77) (be 88)) 88)
      (on-rightmost tm2)
      )))
(test-hook test-list-tm-2)


(defun test-list-tm-3 ()
  (let*(
         (tm1 (mount {7 2 -3}))
         )
    (∧
      (on-leftmost tm1)
      (w tm1 12)
      (s tm1)
      (w tm1 13)
      (s tm1)
      (w tm1 14)
      (equal (tape tm1) {12 13 14})
      (on-rightmost tm1)
      )))
(test-hook test-list-tm-3)

(defun test-list-tm-4 ()
  (let*(
         (tm1 (mount {7 2 -3}))
         )
    (∧
      (s tm1)
      (s tm1)
      (= (r tm1) -3)
      (cue-leftmost tm1)
      (r tm1 7)
      (on-leftmost tm1)
      (¬ (on-rightmost tm1))
      )))
(test-hook test-list-tm-4)


(defun test-list-tm-5 ()
  (let*(
         (tm1 (make-instance 'list-tm))
         )
    (init tm1 {:mount {7 2 -3}})
    (∧
      (on-leftmost tm1)
      (a tm1 21)
      (= (r tm1) 7)
      (on-leftmost tm1)
      (s tm1)
      (= (r tm1) 21)
      (¬ (on-leftmost tm1))
      (s tm1)
      (= (r tm1) 2)
      (a tm1 31)
      (s tm1)
      (= (r tm1) 31)
      (¬ (on-rightmost tm1))
      (s tm1)
      (= (r tm1) -3)
      (on-rightmost tm1)
      (¬ (on-leftmost tm1))
      (a tm1 41)
      (= (r tm1 -3))
      (¬ (on-rightmost tm1))
      (¬ (on-leftmost tm1))
      (s tm1)
      (= (r tm1) 41)
      (on-rightmost tm1)
      (¬ (on-leftmost tm1))
      (cue-leftmost tm1)
      (r tm1 7)
      (on-leftmost tm1)
      (¬ (on-rightmost tm1))
      )))
(test-hook test-list-tm-5)

