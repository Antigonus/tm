#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-tm-interval-0 ()
  (let*(
        (base-tm (mount [0 1 2 3 4 5 6 7]))
        (left-machine (dup base-tm))
        (right-machine (dup base-tm))
        )
    (sn left-machine 3)
    (sn right-machine 5)
    (let(
          (tm-interval (tm-mk 'tm-interval left-machine right-machine))
          )
      (∧
        (on-leftmost tm-interval)
        (= (r tm-interval) 3)
        (s tm-interval)
        (= (r tm-interval) 4)
        (s tm-interval)
        (= (r tm-interval) 5)
        (on-rightmost tm-interval)
        (¬ (s tm-interval))
        )
        )))
(test-hook test-tm-interval-0)
