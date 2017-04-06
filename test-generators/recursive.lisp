#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-recursive-0 ()
  (labels(
           (inc-to-10 (i0 c-success c-fail)
             (let(
                   (i1 (+ i0 1))
                   )
               (if (> i1 10)
                 [c-fail]
                 [c-success i1]
                 )))
           )
    (let(
          (tm (mk 'recursive {:initial 1 :f #'inc-to-10}))
          (result ∅)
          )
      (∀* tm (λ(tm)
               (setf result (cons (r tm) result))
               ))
      (equal result {10 9 8 7 6 5 4 3 2 1})
      )))
(test-hook test-recursive-0)


(defun test-recursive-1 ()
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
(test-hook test-recursive-1)

(defun test-recursive-2 ()
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
(test-hook test-recursive-2)

(defun test-recursive-3 ()
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
(test-hook test-recursive-3)

