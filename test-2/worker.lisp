#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(def-worker counter tm-src () boxed-counter ()
  (s tm-src
    (λ()(incf (unbox boxed-counter)) t)
    (be ∅)
    ))

(defun test-worker-1 ()
  (let(
        (tm (mount {0 1 2 3}))
        (cnt 0)
        )
    (let(
          (worker (counter tm (box cnt)))
          )
      (∧
        (= cnt 0)
        (funcall worker)
        (= cnt 1)
        (funcall worker)
        (= cnt 2)
        (funcall worker)
        (= cnt 3)
        (¬ (funcall worker))
        )
      )))
(test-hook test-worker-1)    
