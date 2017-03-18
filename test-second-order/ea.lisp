#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-ea-0 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {1 2 3}}))
         (tm1 (mk 'ea-tm {:base tm0}))
         (tm2 (entangle tm1))
         )
    (∧
      (typep tm1 'status-active)
      (typep tm2 'status-active)
      (= (r tm1) 1)
      (= (r tm2) 1)
      (= (address tm1) 0)
      (= (address tm2) 0)
      (= (address-rightmost tm1) 2)
      (= (address-rightmost tm2) 2)
      (= (d tm1) 2)
      (= (address-rightmost tm1) 1)
      (= (address-rightmost tm2) 1)
      (s tm2)
      (= (r tm2) 3)
      (= (address tm2) 1)
      (clean-entanglements tm1)
      )))
(test-hook test-ea-0)

(defun test-ea2-0 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {1 2 3}}))
         (tm1 (mk 'ea2-tm {:base tm0}))
         (tm2 (entangle tm1))
         )
    (∧
      (typep tm1 'status-active)
      (typep tm2 'status-active)
      (= (r tm1) 1)
      (= (r tm2) 1)
      (= (address tm1) 0)
      (= (address tm2) 0)
      (= (address-rightmost tm1) 2)
      (= (address-rightmost tm2) 2)
      (= (d tm1) 2)
      (= (address-rightmost tm1) 1)
      (= (address-rightmost tm2) 1)
      (s tm2)
      (= (r tm2) 3)
      (= (address tm2) 1)
      )))
(test-hook test-ea2-0)
