#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-eas*-0 ()
  (let*(
         (tm (mk 'list-nd-tm {:tape { 7  2  -3 -8}}))
         (tm-fill (mk 'list-nd-tm {:tape {77 22 -33}}))
         )
    (∧
      (s tm)
      (eas* tm tm-fill)
      (equal (tape tm) {7 2 77 22 -33 -3 -8})
      (= (r tm) 2)
      )))
(test-hook test-eas*-0)

(defun test-an-0 ()
  (let*(
         (tm (mk 'list-nd-tm {:tape {7 9 11}}))
         (fill (mk 'list-nd-tm {:tape {21 23 25}}))
         )
    (∧
      (an tm 2 fill)
      (equal (tape tm) {7 21 23 9 11})
      (= (r tm) 7)
      (= (r fill) 25)
      )))
(test-hook test-an-0)
