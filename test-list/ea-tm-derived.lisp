#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-ea-a◧-0 ()
  (let*(
         (tm0 (mk 'list-ea-tm {:mount {1 2 3}}))
         )
    (with-mk-entangled tm0
      (λ(tm1)
        (s tm0)
        (a◧ tm0 7)
        (equal (tape tm0) {7 1 2 3})
        (equal (tape tm1) {7 1 2 3})
        (¬ (on-leftmost tm0))
        (on-leftmost tm1)
        ))))
(test-hook test-ea-a◧-0)

(defun test-ea-d-0 ()
  (let*(
         (tm0 (mk 'list-ea-tm {:mount {1 2 3 4}}))
         (tm1 (mk 'list-ea-tm {:mount {-100}}))
         )
    (with-mk-entangled tm0
      (λ(tm2)
        (∧
          (s tm0)
          (sn tm2 2)
          (eq (d tm0 tm1 (be 'ok) (be 'rm) (be 'na) (be 'c)) 'c)
          (on-leftmost tm1)
          (cue-leftmost tm2) ; gets it out of the way

          (d tm0 tm1)
          (equal (tape tm0) {1 2 4})
          (equal (tape tm2) {1 2 4})
          (equal (tape tm1) {-100 3})
          (= (r tm0) 2)
          (= (r tm2) 1)
          (= (r tm1) 3)
          (¬ (on-rightmost tm0))

          (d tm0)
          (equal (tape tm0) {1 2})
          (equal (tape tm2) {1 2})
          (on-rightmost tm0)
          (¬ (on-rightmost tm2))

          (eq (d tm0 ∅ (be 'ok) (be 'rm) (be 'na)) 'rm)
          (equal (tape tm0) {1 2})
          (on-rightmost tm0)

          (cue-leftmost tm0)
          (d tm0)
          (on-rightmost tm0)
          (on-rightmost tm2)
          (on-leftmost tm0)
          (eq (d tm0 ∅ (be 'ok) (be 'rm) (be 'na)) 'rm)
          )))))
(test-hook test-ea-d-0)

(defun test-ea-d◧-0 ()
  (let*(
         (tm0 (mk 'list-ea-tm {:mount {1 2 3}}))
         (tm1 (mk 'list-ea-tm {:mount {-100}}))
         )
    (with-mk-entangled tm0
      (λ(tm2)
        (∧
          (s tm0)
          (¬ (on-leftmost tm0))

          (eq (d◧ tm0 tm1  (be 'ok) (be 'na) (be 'c)) 'c)
          (s tm2)

          (d◧ tm0 tm1)
          (equal (tape tm0) {2 3})
          (equal (tape tm2) {2 3})
          (equal (tape tm1) {-100 1})
          (= (r tm0) 2)
          (= (r tm2) 2)
          (= (r tm1) 1)
          (on-leftmost tm0)
          (on-leftmost tm2)
          (on-rightmost tm1)
          (eq (d◧ tm0 ∅  (be 'ok) (be 'na) (be 'c)) 'c)
          )))))
(test-hook test-ea-d◧-0)
