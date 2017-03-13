#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-status-0 ()
  (let*(
         (tm0 (mk 'list-tm {:tape {1 2 3}}))
         (tm1 (mk 'status-tm {:base tm0}))
         )
    (∧
      (typep tm1 'status-active)
      (= (r tm1) 1)
      (= (s tm1 {:➜ok (be 7) :➜rightmost (be 8)}) 7)
      (= (r tm1) 2)
      (= (s tm1 {:➜ok (be 7) :➜rightmost (be 8)}) 7)
      (= (r tm1) 3)
      (= (s tm1 {:➜ok (be 7) :➜rightmost (be 8)}) 8)
      (= (r tm1 {:➜parked (be 21) :➜ok (be 22)}) 22)
      (= (park tm1 {:➜ok (be 11)}) 11)
      (typep tm1 'status-parked)
      (= (r tm1 {:➜parked (be 21) :➜ok (be 22)}) 21)
      (= (c◧ tm1 {:➜ok (be 41)}) 41)
      (typep tm1 'status-active)
      (= (r tm1 {:➜ok (λ(x)(+ x 99))}) 100)
    )))
(test-hook test-status-0)
