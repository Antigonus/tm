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


;; this is the wrong way to use the worker
;; other implementations may not allow such direct calls
(defun test-worker-0 ()
  (let(
        (tm (mount {0 1 2 3}))
        (cnt 0)
        )
    (∧
      (= cnt 0)
      (counter tm (box cnt))
      (= cnt 1)
      (counter tm (box cnt))
      (= cnt 2)
      (counter tm (box cnt))
      (= cnt 3)
      (¬ (counter tm (box cnt)))
      )
    ))
(test-hook test-worker-0)    

;; this is the correct way to use a worker
(defun test-worker-1 ()
  (let(
        (tm (mount {0 1 2 3}))
        (cnt 0)
        )
    (labels(
          (worker () (counter tm (box cnt)))
          )
      (∧
        (= cnt 0)
        (worker)
        (= cnt 1)
        (worker)
        (= cnt 2)
        (worker)
        (= cnt 3)
        (¬ (worker))
        )
      )))
(test-hook test-worker-1)    
