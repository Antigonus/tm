#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-d*-0 ()
  (let*(
         (tm0 (mk 'list-solo-tm {:tape {1 2 3}}))
         (tm1 (mk 'list-solo-tm {:tape {-100}}))
         )
    (∧
      (d* tm0 tm1)
      (equal (tape tm0) {1})
      (equal (tape tm1) {-100 2 3})
      (on-rightmost tm1)
      )))
(test-hook test-d*-0)

(defun test-dn-0 ()
  (let*(
         (tm0 (mk 'list-solo-tm {:tape {1 2 3 4 5}}))
         (tm1 (mk 'list-solo-tm {:tape {-100}}))
         )
    (∧
      (s tm0)
      (dn tm0 2 tm1)
      (equal (tape tm0) {1 2 5})
      (equal (tape tm1) {-100 3 4})
      (on-rightmost tm1)
      (= (dn tm0 10 ∅ {:➜ok (be -1) :➜rightmost #'echo :➜no-alloc (be -2)}) 9) ; only deleted 1, 9 more to go
      )))
(test-hook test-dn-0)

(defun test-filter-0 ()
  (let*(
         (tm0 (mk 'list-solo-tm {:tape {1 2 3}}))
         (tm1 (mk 'list-solo-tm {:tape {-100}}))
         (pred (λ(tm ct c∅) (declare (ignore tm c∅)) [ct]))
         )
    (∧
      (filter tm0 tm1 pred)
      (equal (tape tm0) {1})
      (equal (tape tm1) {-100 2 3})
      (on-rightmost tm1)
      )))
(test-hook test-filter-0)

(defun test-filter-1 ()
  (let*(
         (tm0 (mk 'list-solo-tm {:tape {1 2 3}}))
         (tm1 (mk 'list-solo-tm {:tape {-100}}))
         (pred (λ(tm ct c∅)(if (oddp (esr tm)) [c∅] [ct]))) ;; need 'esr' here !
         )
    (∧
      (filter tm0 tm1 pred)
      (equal (tape tm0) {1 3})
      (equal (tape tm1) {-100 2})
      (on-rightmost tm1)
      )))
(test-hook test-filter-1)
