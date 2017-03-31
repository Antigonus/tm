#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-recursive-0 ()
  (let(
        (tm (mk 'recursive {:f (increment-to 3)}))
        )
    (∧
      (= (r tm) 0)
      (s tm)
      (= (r tm) 1)
      (s tm)
      (= (r tm) 2)
      (s tm)
      (= (r tm) 3)
      )))
(test-hook test-recursive-0)

(defun test-recursive-1 ()
  (let(
        (tm (mk 'recursive {:f (increment-to 2)}))
        (v #(0 1 2))
        )
    (∀ tm (λ(tm ct c∅)
            (if
              (= (elt v (r tm)) (r tm))
              [ct]
              [c∅]
              )))
    ))
(test-hook test-recursive-1)

(defun test-recursive-2 ()
  (let(
        (tm (mk-interval 1 9 2))
        (v #(8 1 6 3 4 5 2 7 0 9))
        )
    (∀ tm (λ(tm ct c∅)
            (if
              (= (elt v (r tm)) (r tm))
              [ct]
              [c∅]
              )))
    ))
(test-hook test-recursive-2)

