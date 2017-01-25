#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-mk-entangled-0 ()
  (let*(
         (tm0 (mk 'list-nd-tm {:tape {7 2 -3}}))
         (tm1 (mk 'list-nd-tm tm0)) ; this is an entangled copy
         )
    (∧
      (eq (tape tm0) (tape tm1))
      (s tm0)
      (¬ (heads-on-same-cell tm0 tm1))
      (s tm1)
      (heads-on-same-cell tm0 tm1)
      (s tm1)
      (¬ (heads-on-same-cell tm0 tm1))
      )))
(test-hook test-mk-entangled-0)


(defun test-recycle-entangled-0 ()
  (let*(
         (tm0 (mk 'list-nd-tm {:tape {7 2 -3}}))
         (tm1 (mk 'list-nd-tm {:tape {11 22 33}}))
         (tm2 (mk 'list-nd-tm tm0))
         )
    (∧
      (eq (tape tm0) (tape tm2))
      (¬ (eq (tape tm1) (tape tm2)))
      (init tm2 tm1)
      (¬ (eq (tape tm0) (tape tm2)))
      (eq (tape tm1) (tape tm2))

      (s tm2)
      (¬ (heads-on-same-cell tm1 tm2))
      (s tm1)
      (heads-on-same-cell tm1 tm2)
      (s tm1)
      (¬ (heads-on-same-cell tm1 tm2))
      )))
(test-hook test-recycle-entangled-0)

(defun test-r◧-0 ()
  (let*(
         (tm0 (mk 'list-nd-tm {:tape {7 2 -3}}))
         (tm1 (mk 'list-nd-tm {:tape {11 22 33}}))
         (tm2 (mk 'list-nd-tm tm0))
         )
    (∧
      (= (r◧ tm0) 7)
      (= (r◧ tm1) 11)
      (= (r◧ tm2) 7)
      (w◧ tm2 9)
      (= (r◧ tm0) 9)
      )))
(test-hook test-r◧-0)

(defun test-s≠-0 ()
  (let*(
         (tm0 (mk 'list-nd-tm {:tape {3 5 7 9 11}}))
         (tm1 (mk 'list-nd-tm tm0))
         )
    (∧
      (sn tm0 3)
      (= (s≠ tm1 tm0 {:➜ok (be 1) :➜rightmost (be 2) :➜bound (be 3)}) 1)
      (= (s≠ tm1 tm0 {:➜ok (be 1) :➜rightmost (be 2) :➜bound (be 3)}) 1)
      (= (s≠ tm1 tm0 {:➜ok (be 1) :➜rightmost (be 2) :➜bound (be 3)}) 1)
      (= (s≠ tm1 tm0 {:➜ok (be 1) :➜rightmost (be 2) :➜bound (be 3)}) 3)
      )))
(test-hook test-s≠-0)

(defun test-s≠-1 ()
  (let*(
         (tm0 (mk 'list-nd-tm {:tape {3 5 7 9 11}}))
         (tm1 (mk 'list-nd-tm tm0))
         )
    (∧
      (s tm1)
      (= (s≠ tm1 tm0 {:➜ok (be 1) :➜rightmost (be 2) :➜bound (be 3)}) 1)
      (= (s≠ tm1 tm0 {:➜ok (be 1) :➜rightmost (be 2) :➜bound (be 3)}) 1)
      (= (s≠ tm1 tm0 {:➜ok (be 1) :➜rightmost (be 2) :➜bound (be 3)}) 1)
      (= (s≠ tm1 tm0 {:➜ok (be 1) :➜rightmost (be 2) :➜bound (be 3)}) 2)
      )))
(test-hook test-s≠-1)

(defun test-a◨ ()
  (let*(
         (tm0 (mk 'list-nd-tm {:tape {3 5 7 9 11}}))
         (tm1 (mk 'list-nd-tm tm0))
         )
    (∧
      (a◨ tm1 13)
      (a◨ tm0 15)
      (equal (tape tm0) {3 5 7 9 11 13 15})
      (on-leftmost tm0)
      (on-leftmost tm1)
      )))
(test-hook test-a◨)

