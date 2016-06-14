#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-tm-mk-list-0 ()
  (let*(
         (tm0 (mk 'tm-list))
         (tm1 (mount {7 2 -3}))
         (tm2 (mk (type-of tm0)))
         )
    (∧
      (typep tm2 'tm-list)
      (= (r tm0 (be 1) (be 2)) 2)
      (eql (r tm1) 7)
      (typep (tape tm1) 'cons)
      (s tm1)(s tm1)
      (on-rightmost tm1)
      )))
(test-hook test-tm-mk-list-0)


