#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package :tm)

(defun test-tm-mk-list-0 ()
  (let*(
         (tm0 (tm-mk 'tm-list))
         (tm1 (tm-mk ∅ (list 7 2 -3)))
         (tm2 (tm-mk 'tm-list tm1))
         )
    (∧
      (eq (r tm0) 'list)
      (eql (r tm1) 7)
      (eq (car (tape tm2)) 'list)
      (on-rightmost tm2)
      )))
(test-hook test-tm-mk-list-0)


