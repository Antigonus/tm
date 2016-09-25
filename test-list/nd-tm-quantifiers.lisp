#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-s-together-0 ()
  (let*(
         (tm0 (mk 'list-nd-tm :mount { 7  2  -3 -8}))
         (tm1 (mk 'list-nd-tm :mount {77 22 -33}))
         (tms (mount {tm0 tm1}))
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

(defun test-esnr-0 ()
  (let*(
         (tm0 (mk 'list-nd-tm :mount { 7  2  -3 -8}))
         )
    (∧
      (= (esnr tm0 2) -3)
      (esnw tm0 2 333)
      (= (esnr tm0 2) 333)
      (= (esnr tm0 3) -8)
      (= (esnr tm0 3 (be -1) (be -2)) -1)
      (= (esnr tm0 4 (be 111) (be 222)) 222)
      (= (esnw tm0 4 -11 (be 777) (be 999)) 999)
      )))
(test-hook test-esnr-0)

