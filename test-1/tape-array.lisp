#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-tape-ref-array-realloc-0 ()
  (let(tp-empty tp0 tp1 tp2 tp3)
    (∧
      (a◨<tape-ref-array-realloc> tp0 0)

      (a◨<tape-ref-array-realloc> tp1 0)
      (a◨<tape-ref-array-realloc> tp1 1)

      (a◨<tape-ref-array-realloc> tp2 0)
      (a◨<tape-ref-array-realloc> tp2 1)
      (a◨<tape-ref-array-realloc> tp2 2)
      
      (a◨<tape-ref-array-realloc> tp3 0)
      (a◨<tape-ref-array-realloc> tp3 1)
      (a◨<tape-ref-array-realloc> tp3 2)
      (a◨<tape-ref-array-realloc> tp3 3)

      (typep tp-empty 'null)
      (typep tp0 'tape-ref-array-realloc-max-0)
      (typep tp1 'tape-ref-array-realloc-max-1)
      (typep tp2 'tape-ref-array-realloc-max-2)
      (typep tp3 'tape-ref-array-realloc-max-n)

      (= (r<tape-ref-array-realloc> tp-empty {:➜empty (λ()5)}) 5)
      (= (r<tape-ref-array-realloc> tp0 {:➜ok (λ(x)(if (= x 0) 7 12))}) 7)
      (= (r<tape-ref-array-realloc> tp0 {:address 1 :➜empty (λ()5)}) 5)

      (= (r<tape-ref-array-realloc> tp1) 0)
      (= (r<tape-ref-array-realloc> tp1 {:address 1}) 1)
      (= (r<tape-ref-array-realloc> tp1 {:address 2 :➜empty (λ()5)}) 5)

      (= (r<tape-ref-array-realloc> tp2) 0)
      (= (r<tape-ref-array-realloc> tp2 {:address 1}) 1)
      (= (r<tape-ref-array-realloc> tp2 {:address 2}) 2)
      (= (r<tape-ref-array-realloc> tp2 {:address 3 :➜empty (λ()5)}) 5)

      (= (r<tape-ref-array-realloc> tp3) 0)
      (= (r<tape-ref-array-realloc> tp3 {:address 1}) 1)
      (= (r<tape-ref-array-realloc> tp3 {:address 2}) 2)
      (= (r<tape-ref-array-realloc> tp3 {:address 3}) 3)
      (= (r<tape-ref-array-realloc> tp3 {:address 4 :➜empty (λ()5)}) 5)

      )))
(test-hook test-tape-ref-array-realloc-0)


;; need to add a test for expanding with intermediate empty cells
