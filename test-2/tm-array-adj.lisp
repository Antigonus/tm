#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-tm-mk-array-0 ()
  (let*(
         (tm0 (tm-mk 'tm-array-adj))
         (tm1 (tm-mk 'tm-array-adj #(7 2 -3)))
         (tm2 (tm-mk 'tm-array-adj))
         )
    (cue-to tm2 tm1)
    (and
      (eq (r tm0) 'array-adj)
      (eql (r tm1) 7)
      (heads-on-same-cell tm1 tm2)
      )))
(test-hook test-tm-mk-array-0)

(defun test-cue-array-adj-0 ()
  (let(
        (x (tm-mk 'tm-array-adj #(a b c)))
        (y (tm-mk 'tm-array-adj))
        )
    (and
      (eq (r x) 'a)
      (eq (r y) 'array-adj)
      (s x)
      (cue-to y x)
      (eq (r x) 'b)
      (eq (r y) 'b)
      (heads-on-same-cell x y)
      (cue-rightmost x)
      (eq (r x) 'c)
      )))
(test-hook test-cue-array-adj-0)

(defun test-s-array-0 ()
  (let*(
         (y #(1 2 (3 4) 5))
         (ytm (tm-mk 'tm-array-adj y))
         )
    (and
      (s ytm)
      (s ytm)
      (equal '(3 4) (r ytm))
      (s ytm)
      (not (s ytm))
      )))
(test-hook test-s-array-0) 

