#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-tape-tiled-natural-0 ()
  (let(
        (ttn (make-instance 'tape-tiled-natural))
        )
    (setf (natural ttn) 0)
    (setf (length-tile ttn) 4)
    (write-tiled-natural ttn 0 #xA)
    (∧
      (= (natural ttn) #xA)
      (= (read-tiled-natural ttn 0) #xA)
      )))

(test-hook test-tape-tiled-natural-0)


(defun test-tape-tiled-natural-1 ()
  (let*(
         (tp0 (mk 'tape-tiled-natural ∅))
         (tp1 (mk 'tape-tiled-natural {1 2 3}))
         (tp2 (mk 'tape-tiled-natural #(4 5 6)))
         (tp3 (mk 'tape-tiled-natural tp1))
         )
    (∧
      (typep tp0 'tape-empty)
      (= (natural tp1) #x030201)
      (= (natural tp2) #x060504)
      (= (natural tp3) #x030201)
      )))
(test-hook test-tape-tiled-natural-1)

#|

(defun test-tape-tiled-natural-1 ()
  (let*(
         (tp1 (mk 'tape-tiled-natural {1 2 3}))
         )
    (∧
      (= (e-s*r tp1) 1)
      (= (e-s*sr tp1) 2)
      (e-s*w tp1 11)
      (e-s*sw tp1 12)
      (equal (tm::cons-list tp1) {11 12 3})
      )))
(test-hook test-tape-tiled-natural-1)

(defun test-tape-tiled-natural-2 ()
  (let*(
         (tp2 (mk 'tape-tiled-natural #(4 5 6)))
         )

    (let*(
           (c0 (leftmost tp2))
           (c1 (right-neighbor c0))
           (c2 (right-neighbor c1))
           (v  (right-neighbor c2 {:➜ok (be 100) :➜rightmost (be 200)}))
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
(test-hook test-tape-tiled-natural-2)

(defun test-tape-tiled-natural-3 ()
  (let*(
         (tp10 (mk 'tape-tiled-natural ∅))
         (tp20 (mk 'tape-tiled-natural ∅))
         (tp1 (mk 'tape-tiled-natural {1 2 3}))
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
        (equal (tm::cons-list tp10) {77})
        (equal (tm::cons-list tp1) {0 81 1 2 3})
        (equal (tm::cons-list tp20) {9})
        ))))
(test-hook test-tape-tiled-natural-3)

(defun test-tape-tiled-natural-4 ()
  (let*(
         (tp0 (mk 'tape-tiled-natural ∅))
         (tp1 (mk 'tape-tiled-natural {1 2 3}))
         (tp2 (mk 'tape-tiled-natural #(4 5 6)))
         (tp3 (mk 'tape-tiled-natural tp1))
         (tp4 (mk 'tape-tiled-natural {17 18 19}))
         )
    (let*(
           (c0 (leftmost tp2))
           (c1 (right-neighbor c0))
           )
      (∧
        (epd<tape> tp0 {:➜ok (be ∅) :➜rightmost (be t)})
        (epd<tape> tp1 {:➜ok (λ(c)(= (r<cell> c) 1)) :➜rightmost (be ∅)})
        (= (r<cell> c1) 5)
        (d<cell> c1 {:➜ok (λ(c)(= (r<cell> c) 6)) :➜rightmost (be ∅)})
        (d<cell> c1 {:➜ok (be ∅) :➜rightmost (be t)})
        (d.<cell> (leftmost tp3) {:➜ok (λ(c)(= (r<cell> c) 1)) :➜rightmost (be ∅)})
        (d.<cell> (leftmost tp3) {:➜ok (λ(c)(= (r<cell> c) 2)) :➜rightmost (be ∅)})
        (d.<cell> (leftmost tp3) {:➜ok (be ∅) :➜rightmost (be t)})
        (e-s*d.<tape> tp4 {:➜ok (λ(c)(= (r<cell> c) 17)) :➜rightmost (be ∅)})
        (e-s*d.<tape> tp4 {:➜ok (λ(c)(= (r<cell> c) 18)) :➜rightmost (be ∅)})
        (e-s*d.<tape> tp4 {:➜ok (λ(c)(= (r<cell> c) 19)) :➜rightmost (be ∅)})
        (typep tp4 'tape-empty)
        ))))
(test-hook test-tape-tiled-natural-4)
|#
