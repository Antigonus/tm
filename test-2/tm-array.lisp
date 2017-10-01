#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#
(in-package #:tm)

(defun test-tm-ref-array-realloc-0 ()
  (let(
        (tm0 (mk 'tm-ref-array-realloc))
        )
    (∧
      (= (eur tm0 {:➜ok (be 1) :➜empty (be 3)}) 3)
      (euw tm0 7)
      (= (eur tm0) 7)
      )))
(test-hook test-tm-ref-array-realloc-0)

(defun test-tm-ref-array-realloc-1 ()
  (let(
        (tm0 (mk 'tm-ref-array-realloc))
        )
    (∧
      (euw tm0 7 {:address 2})
      (euw tm0 6 {:address 1})
      (euw tm0 5 {:address 0})
      (typep tm0 'tm-parked)
      (u tm0)
      (= (r tm0) 5)
      (w tm0 50)
      (s tm0)
      (= (r tm0) 6)
      (w tm0 60)
      (s tm0)
      (= (r tm0) 7)
      (= (s tm0 {:➜bound (be 12) :➜ok (be 11)}) 12)
      (s tm0 {:Δ -1})
      (= (r tm0) 60)
      (s tm0 {:Δ -1})
      (= (r tm0) 50)
      (= (s tm0 {:Δ -1 :➜bound (be 22) :➜ok (be 21)}) 22)
      )))
(test-hook test-tm-ref-array-realloc-1)



;; need to add a test for expanding with intermediate empty cells
