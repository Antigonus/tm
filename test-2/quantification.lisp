#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#
(in-package #:tm)

(defun test-quantification-0 ()
  (let(
        (tm0 (mk 'tm-ref-array-realloc))
        )
    (∧
      (euw tm0 7 {:address 2})
      (euw tm0 5 {:address 1})
      (euw tm0 3 {:address 0})
      (typep tm0 'tm-parked)
      (∀ tm0 (λ(tm0 ➜∅ ➜t)(if (oddp (r tm0)) [➜t] [➜∅])))
      )))
(test-hook test-quantification-0)


;; need to add a test for expanding with intermediate empty cells
