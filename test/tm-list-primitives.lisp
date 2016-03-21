#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package :tm)

(defun test-tm-mk-list-0 ()
  (let*(
         (tm0 (tm-mk 'tm-list))
         (tm1 (tm-mk 'cons (list 7 2 -3)))
         (tm2 (tm-mk 'tm-list tm1))
         )
    (∧
      (eq (r tm0) 'list)
      (eql (r tm1) 7)
      (eq (car (tape tm2)) 'list)
      (on-rightmost tm2)
      )))
(test-hook test-tm-mk-list-0)


(defun test-tm-list-primitives-1 ()
  (let(
        (k (tm-mk 'cons (list 1 2 3)))
        )
    (∧
      (w k 7)
      (= (r k) 7)
      (so k)
      (= (r k) 2)
      (cue-leftmost k)
      (= (r k) 7)
      )))
(test-hook test-tm-list-primitives-1)

(defun test-tm-list-cue-0 ()
  (let(
        (x (tm-mk-list '(a b c)))
        (y (tm-mk-list))
        )
    (and
      (eq (r x) 'a)
      (eq (r y) 'list)
      (s x)
      (cue-to y x) 
      (eq (r x) 'b)
      (eq (r y) 'b)
      (heads-on-same-cell x y)
      (cue-rightmost x)
      (eq (r x) 'c)
      )))
(test-hook test-tm-list-cue-0)

(defun test-tm-list-s-0 ()
  (let*(
         (y '(1 2 (3 4) 5))
         (ytm (tm-mk-list y))
         )
    (and
      (s ytm)
      (s ytm)
      (equal '(3 4) (r ytm))
      (s ytm)
      (not (s ytm))
      )))
(test-hook test-tm-list-s-0) 

(defun test-tm-list-a-0 ()
  (let(
        (k (tm-mk-list))
        )
    (∧
      (equal (tape k) '(list))
      (as k 12)
      (as k 13)
      (as k 14)
      (equal (tape k) '(list 12 13 14))
      )))
(test-hook test-tm-list-a-0)

(defun test-d-0 ()
  (let*(
         (a (list 1 2 3))
         (tm1 (tm-mk-list a))
         )
    (d tm1)
    (equal
      (tape tm1)
      '(1 3)
      )))
(test-hook test-d-0)


(defun test-tm-list-d-1 ()
  (let(
        (k (tm-mk-list))
        )
    (∧
      (equal (tape k) '(list))
      (as k 12)
      (as k 13)
      (as k 14)
      (equal (tape k) '(list 12 13 14))
      (cue-leftmost k)
      (so k)
      (d k)
      (equal (tape k) '(list 12 14))
      (d k)
      (equal (tape k) '(list 12))
      (on-rightmost k)
      )))
(test-hook test-tm-list-d-1)
