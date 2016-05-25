#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-tm-region-0 ()
  (let*(
        (base-tm (mount [0 1 2 3 4 5 6 7]))
        (left-machine (dup base-tm))
        (right-machine (dup base-tm))
        )
    (sn left-machine 3)
    (sn right-machine 5)
    (let(
          (tm-region (mk 'tm-region left-machine right-machine))
          )
      (∧
        (on-leftmost tm-region)
        (= (r tm-region) 3)
        (s tm-region)
        (= (r tm-region) 4)
        (s tm-region)
        (= (r tm-region) 5)
        (on-rightmost tm-region)
        (¬ (s tm-region))
        )
        )))
(test-hook test-tm-region-0)
