#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-bilist-0 ()
  (let(
        (l (make-bilist {1 2 3}))
        )
    (let*(
           (n0 l)
           (n1 (binode-right-neighbor n0))
           (n2 (binode-right-neighbor n1))
           )
      (∧
        (= (binode-instance n0) 1)
        (= (binode-instance n1) 2)
        (= (binode-instance n2) 3)
        (¬ (binode-right-neighbor n2))
        (¬ (binode-left-neighbor n0))
        (eq (binode-left-neighbor n1) n0)
        (eq (binode-left-neighbor n2) n1)
        ))))
(test-hook test-bilist-0)

(defun test-bilist-1 ()
  (let(
        (tm0 (mk 'bilist-nd-tm {:tape {7 2 -3}}))
        )
    (with-entangled tm0
      (λ(tm1)
        (∧
          (eq (tape tm0) (tape tm1))
          (s tm0)
          (¬ (heads-on-same-cell tm0 tm1))
          (s tm1)
          (heads-on-same-cell tm0 tm1)
          (s tm1)
          (¬ (heads-on-same-cell tm0 tm1))
          (¬ (s tm1))
          (-s tm1)
          (heads-on-same-cell tm0 tm1)
          (-s* tm1)
          (= (r tm1) 7)
          (s tm1)
          (= (r tm1) 2)
          (s tm1)
          (= (r tm1) -3)
          (¬ (s tm1))
          (-s tm1)
          (= (r tm1) 2)
          (-s tm1)
          (= (r tm1) 7)
          (¬ (-s tm1))
          )))))
(test-hook test-bilist-1)

(defun test-bilist-2 ()
  (let(
        (tm0 (mk 'bilist-solo-tm {:tape {7 2 -3 4}}))
        )
    (∧
      (s tm0)
      (d tm0)
      (-s tm0)
      (= (r tm0) 7)
      (s tm0)
      (= (r tm0) 2)
      (s tm0)
      (= (r tm0) 4)
      (¬ (s tm0))
      )))
(test-hook test-bilist-2)

(defun test-bilist-3 ()
  (let(
        (tm0 (mk 'bilist-solo-tm {:tape {7 2 -3 4}}))
        )
    (∧
      (s tm0)
      (= (epd tm0) 7)
      (= (r tm0) 2)

      (s tm0)
      (= (epd tm0) 2)
      (= (r tm0) -3)

      (s tm0)
      (= (epd tm0) -3)
      (= (r tm0)  4)

      (¬ (s tm0))
      )))
(test-hook test-bilist-3)

(defun test-bilist-4 ()
  (let(
        (tm0 (mk 'bilist-solo-tm {:tape {7}}))
        )
    (∧
      (= (-s tm0 {:➜ok (be 3) :➜leftmost (be 4)}) 4)
      (= (epd tm0 ∅ {:➜ok (be 22) :➜collision (be 55)}) 55)
      )))
(test-hook test-bilist-4)

(defun test-bilist-5 ()
  (let(
        (tm0 (mk 'bilist-solo-tm {:tape {7 8}}))
        )
    (∧
      (= (s tm0 {:➜ok (be 3) :➜leftmost (be 4)}) 3)
      (= (-d tm0) 7)
      (= (-d tm0 ∅ {:➜ok (be 22) :➜collision (be 55) :➜leftmost (be 27)}) 27)
      (¬ (s tm0))
      (¬ (-s tm0))
      )))
(test-hook test-bilist-5)
