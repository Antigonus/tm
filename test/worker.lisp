#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package :tm)


(def-worker counter tm-src () boxed-counter ()
  (s tm-src
    (λ()(incf (unbox boxed-counter)) t)
    (be ∅)
    ))


;; this is the wrong way to use the worker
;; other implementations may not allow such direct calls
(defun test-worker-0 ()
  (let(
        (tm (mk-tm 'tm-list {0 1 2 3}))
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
        (tm (mk-tm 'tm-list {0 1 2 3}))
        (cnt 0)
        )
    (setf 
      (symbol-function 'work) 
      (connect #'counter tm (box cnt))
      )
    (∧
      (= cnt 0)
      (work)
      (= cnt 1)
      (work)
      (= cnt 2)
      (work)
      (= cnt 3)
      (¬ (work))
      )
    ))
(test-hook test-worker-1)    
  
