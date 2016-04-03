#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-as-0 ()
  (let*(
         (tm0 (mount {7 9 11}))
         (tm1 (dup tm0))
         )
    (as tm0 8)
    (s tm0)
    (as tm0 10)
    (and
      (= (r tm0) 10)
      (equal (tape tm1) '(7 8 9 10 11)) ; head is not for public use
      )))
