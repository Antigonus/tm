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


(def-worker counter-2 tm-src tm-dst boxed-counter (cont-ok cont-◨)
  (as tm-dst (unbox boxed-counter))
  (s tm-src 
    (λ()
      (incf (unbox boxed-counter)) 
      (funcall cont-ok)
      )
    cont-◨
    ))

(defun test-worker-2 ()
  (let*(
         (tm-src (mount {0 1 2 3}))
         (tm-dst (tm-mk 'tm-list))
         (cnt 0)
         (worker (counter-2 tm-src tm-dst (box cnt)))
         )
    (⟳ (λ(cont-loop cont-return)
         (funcall worker cont-loop cont-return)
         ))
    (∧
      (= cnt 3)
      (equal
        (unmount tm-src)
        (cdr (unmount tm-dst))
        ))))
(test-hook test-worker-2)


