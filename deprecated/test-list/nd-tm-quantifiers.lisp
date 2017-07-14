#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)


#| s-together is being turned into a transform

(defun test-s-together-0 ()
  (let*(
         (tm0 (mk 'list-nd-tm {:tape { 7  2  -3 -8}}))
         (tm1 (mk 'list-nd-tm {:tape {77 22 -33}}))
         (tms (mk 'list-tm    {:tape {tm0 tm1}}))
         )
    (∧
      (s-together tms)
      (= (r tm0) 2)
      (= (r tm1) 22)
      (s-together tms)
      (= (r tm0) -3)
      (= (r tm1) -33)
      (= (s-together tms (be 100) (be 200)) 200)
      (= (r tm0) -3)
      (= (r tm1) -33)
      )))
(test-hook test-s-together-0)
|#

(defun test-esnr-0 ()
  (let*(
         (tm0 (mk 'list-nd-tm {:tape { 7  2  -3 -8}}))
         )
    (∧
      (= (esnr tm0 2) -3)
      (esnw tm0 2 333)
      (= (esnr tm0 2) 333)
      (= (esnr tm0 3) -8)
      (= (esnr tm0 3 {:➜ok (be -1) :➜rightmost (be -2)}) -1)
      (= (esnr tm0 4 {:➜ok (be 111) :➜rightmost (be 222)}) 222)
      (= (esnw tm0 4 -11 {:➜ok (be 777) :➜rightmost (be 999)}) 999)
      )))
(test-hook test-esnr-0)

