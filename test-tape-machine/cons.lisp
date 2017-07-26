#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-tape-cons-0 ()
  (let*(
         (tp0 (mk 'tape-cons ∅))
         (tp1 (mk 'tape-cons {1 2 3}))
         (tp2 (mk 'tape-cons #(4 5 6)))
         (tp3 (mk 'tape-cons tp1))
         )
    (∧
      (typep tp0 'tape-empty)
      (equal (tm::cons-list tp1) {1 2 3})
      (equal (tm::cons-list tp2) {4 5 6})
      (equal (tm::cons-list tp3) {1 2 3})
      )))
(test-hook test-tape-cons-0)

(defun test-tape-cons-1 ()
  (let*(
         (tp1 (mk 'tape-cons {1 2 3}))
         )
    (∧
      (= (◧r tp1) 1)
      (= (◧sr tp1) 2)
      (◧w tp1 11)
      (◧sw tp1 12)
      (equal (tm::cons-list tp1) {11 12 3})
      )))
(test-hook test-tape-cons-1)

(defun test-tape-cons-2 ()
  (let*(
         (tp2 (mk 'tape-cons #(4 5 6)))
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
        (equal (tm::cons-list tp2) {41 51 61})
        ))))
(test-hook test-tape-cons-2)

(defun test-tape-cons-3 ()
  (let*(
         (tp10 (mk 'tape-cons ∅))
         (tp20 (mk 'tape-cons ∅))
         (tp1 (mk 'tape-cons {1 2 3}))
         )
    (let*(
           (c0 (make-instance 'cell-cons :cons-cell (cons 77 79)))
           (c1 (make-instance 'cell-cons :cons-cell (cons 81 83)))
           )
      (epa<cell> tp10 c0)
      (epa<cell> tp1 c1)
      (epa<instance> tp20 9)
      (epa<instance> tp1 0)
      (∧
        (equal (tm::cons-list tp10) {77})
        (equal (tm::cons-list tp1) {0 81 1 2 3})
        (equal (tm::cons-list tp20) {9})
        ))))
(test-hook test-tape-cons-3)

(defun test-tape-cons-4 ()
  (let*(
         (tp0 (mk 'tape-cons ∅))
         (tp1 (mk 'tape-cons {1 2 3}))
         (tp2 (mk 'tape-cons #(4 5 6)))
         (tp3 (mk 'tape-cons tp1))
         (tp4 (mk 'tape-cons {17 18 19}))
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
(test-hook test-tape-cons-4)

(defun test-tape-cons-5 ()
  (let*(
         (tp0 (mk 'tape-cons ∅))
         (tp01 (mk 'tape-cons {'a 'b}))
         (tp11 (mk 'tape-cons {1 2 3}))
         (tp12 (mk 'tape-cons {1 2 3}))
         (tp1 (mk 'tape-cons {1 2 3}))
         (tp2 (mk 'tape-cons #(4 5 6 7 8)))
         (tp4 (mk 'tape-cons {17 18 19}))
         v1
         cell1 cell2
         flag1 flag2 flag3 flag4 flag5 flag6 flag7 flag8 flag9 flag10
         )
    (shallow-copy-no-topo tp0 tp1)
    (setf flag1 (typep tp0 'tape-empty))

    (shallow-copy-no-topo tp11 tp0)
    (setf flag2 (equal (cons-list tp11) {∅ ∅ ∅}))

    (shallow-copy-no-topo tp12 tp01)
    (setf flag3 (equal (cons-list tp12) {'a 'b ∅}))

    (shallow-copy-no-topo tp1 tp2)
    (setf flag4 (equal (cons-list tp1) {4 5 6}))

    (setf cell1 (shallow-copy-topo tp1 tp2))
    (setf flag5 (equal (cons-list tp1) {4 5 6 7 8}))
    (setf flag6 (= (r<cell> cell1) 8))

    (setf v1 (shallow-copy-topo tp1 tp0 {:➜ok (be 21) :➜empty (be 22)}))
    (setf flag7 (typep tp1 'tape-empty))
    (setf flag8 (= v1 22))

    (setf cell2 (shallow-copy-topo tp2 tp4))
    (setf flag9 (equal (cons-list tp2) {17 18 19}))
    (setf flag10 (= (r<cell> cell2) 19))

    (∧ flag1 flag2 flag3 flag4 flag5 flag6 flag7 flag8 flag9 flag10)
    ))
(test-hook test-tape-cons-5)
      
