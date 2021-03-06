#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-tape-ref-array-realloc-0 ()
  (let*(
         (tp0 (mk 'tape-ref-array-realloc ∅))
         (tp1 (mk 'tape-ref-array-realloc {1 2 3} {:maximum-address 3}))
         (tp2 (mk 'tape-ref-array-realloc #(4 5 6)))
         (tp3 (mk 'tape-ref-array-realloc tp1))
         )
    (let(
          (arr1 (the-array tp1))
          (arr2 (the-array tp2))
          (arr3 (the-array tp3))
          )
      (let(
            (list1 (coerce arr1 'list))
            (list2 (coerce arr2 'list))
            (list3 (coerce arr3 'list))
            )
        (∧
          (typep tp0 'tape-empty)
          (equal list1 {1 2 3 ∅})
          (equal list2 {4 5 6})
          (equal list3 {1 2 3 ∅})
          )))))
(test-hook test-tape-ref-array-realloc-0)

(defun test-tape-ref-array-realloc-1 ()
  (let*(
         (tp1 (mk 'tape-ref-array-realloc {1 2 3}))
         )
    (∧
      (= (◧r tp1) 1)
      (= (◧sr tp1) 2)
      (◧w tp1 11)
      (◧sw tp1 12)
      (equal (coerce (tm::the-array tp1) 'list) {11 12 3})
      )))
(test-hook test-tape-ref-array-realloc-1)

(defun test-tape-ref-array-realloc-2 ()
  (let*(
         (tp2 (mk 'tape-ref-array-realloc #(4 5 6)))
         )

    (let*(
           (c0 (bound-left tp2))
           (c1 (right-neighbor c0))
           (c2 (right-neighbor c1))
           (v  (right-neighbor c2 {:➜ok (be 100) :➜bound-right (be 200)}))
           )
      (∧
        (= (r<cell> c0) 4)
        (w<cell> c0 41)
        (= (r<cell> c1) 5)
        (w<cell> c1 51)
        (= (r<cell> c2) 6)
        (w<cell> c2 61)
        (= v 200)
        (equal (coerce (tm::the-array tp2) 'list) {41 51 61})
        ))))
(test-hook test-tape-ref-array-realloc-2)

#| no topo ops for array
(defun test-tape-ref-array-realloc-3 ()
  (let*(
         (tp10 (mk 'tape-ref-array-realloc ∅))
         (tp20 (mk 'tape-ref-array-realloc ∅))
         (tp1 (mk 'tape-ref-array-realloc {1 2 3}))
         )
    (let*(
           (c0 (make-instance 'cell-list :cons-cell (cons 77 79)))
           (c1 (make-instance 'cell-list :cons-cell (cons 81 83)))
           )
      (epa<cell> tp10 c0)
      (epa<cell> tp1 c1)
      (epa<instance> tp20 9)
      (epa<instance> tp1 0)
      (∧
        (equal (coerce (tm::the-array tp10) 'list) {77})
        (equal (coerce (tm::the-array tp1) 'list) {0 81 1 2 3})
        (equal (coerce (tm::the-array tp20) 'list)  {9})
        ))))
(test-hook test-tape-ref-array-realloc-3)

(defun test-tape-ref-array-realloc-4 ()
  (let*(
         (tp0 (mk 'tape-ref-array-realloc ∅))
         (tp1 (mk 'tape-ref-array-realloc {1 2 3}))
         (tp2 (mk 'tape-ref-array-realloc #(4 5 6)))
         (tp3 (mk 'tape-ref-array-realloc tp1))
         (tp4 (mk 'tape-ref-array-realloc {17 18 19}))
         )
    (let*(
           (c0 (bound-left tp2))
           (c1 (right-neighbor c0))
           )
      (∧
        (epd<tape> tp0 {:➜ok (be ∅) :➜bound-right (be t)})
        (epd<tape> tp1 {:➜ok (λ(c)(= (r<cell> c) 1)) :➜bound-right (be ∅)})
        (= (r<cell> c1) 5)
        (d<cell> c1 {:➜ok (λ(c)(= (r<cell> c) 6)) :➜bound-right (be ∅)})
        (d<cell> c1 {:➜ok (be ∅) :➜bound-right (be t)})
        (d.<cell> (bound-left tp3) {:➜ok (λ(c)(= (r<cell> c) 1)) :➜bound-right (be ∅)})
        (d.<cell> (bound-left tp3) {:➜ok (λ(c)(= (r<cell> c) 2)) :➜bound-right (be ∅)})
        (d.<cell> (bound-left tp3) {:➜ok (be ∅) :➜bound-right (be t)})
        (◧d.<tape> tp4 {:➜ok (λ(c)(= (r<cell> c) 17)) :➜bound-right (be ∅)})
        (◧d.<tape> tp4 {:➜ok (λ(c)(= (r<cell> c) 18)) :➜bound-right (be ∅)})
        (◧d.<tape> tp4 {:➜ok (λ(c)(= (r<cell> c) 19)) :➜bound-right (be ∅)})
        (typep tp4 'tape-empty)
        ))))
(test-hook test-tape-ref-array-realloc-4)
|#
