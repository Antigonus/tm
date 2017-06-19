#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-tape-0 ()
  (let*(
         (tp0 (mk 'tape-cons ∅))
         (tp00 (mk 'tape-cons tp0))
         (tp1 (mk 'tape-cons #('a)))
         (tp2 (mk 'tape-cons #('a 'b)))
         (tp3 (mk 'tape-cons #('a 'b 'c)))
         )
    (∧
      (typep tp0 'tape-empty)
      (typep tp00 'tape-empty)

      (maximum-address tp0 {:➜ok (be ∅) :➜empty (be t)})
      (maximum-address tp1 {:➜ok (λ(maximum)(= maximum 0)) :➜empty (be ∅)})
      (= (maximum-address tp2) 1)
      (= (maximum-address tp3) 2)

      (◧r tp0 {:➜ok (be ∅) :➜empty (be t)})
      (◧r tp1 {:➜ok (be t) :➜empty (be ∅)})

      (◧sr tp00 {:➜ok (be ∅) :➜rightmost (be ∅) :➜empty (be t)})
      (◧sr tp1  {:➜ok (be ∅) :➜rightmost (be t) :➜empty (be ∅)})
      (◧sr tp2  {:➜ok (be t) :➜rightmost (be ∅) :➜empty (be ∅)})

      (◧w tp0 12 {:➜ok (be ∅) :➜empty (be t)})
      (◧w tp1 13 {:➜ok (be t) :➜empty (be ∅)})

      (◧sw tp0 14 {:➜ok (be ∅) :➜rightmost (be ∅) :➜empty (be t)})
      (◧sw tp1 15 {:➜ok (be ∅) :➜rightmost (be t) :➜empty (be ∅)})
      (◧sw tp2 16 {:➜ok (be t) :➜rightmost (be ∅) :➜empty (be ∅)})

      (es*r tp0 {:➜ok (be ∅) :➜empty (be t)})
      (◨r tp00 {:➜ok (be ∅) :➜empty (be t)})
      (es*w tp0 16 {:➜ok (be ∅) :➜empty (be t)})
      (◨w tp00 17 {:➜ok (be ∅) :➜empty (be t)})

      (leftmost tp00 {:➜ok (be ∅) :➜empty (be t)})
      (leftmost tp1 {:➜ok (be t) :➜empty (be ∅)})

      (epd<tape> tp0 {:➜ok (be ∅) :➜rightmost (be t)})
      (◨d<tape> tp00 {:➜ok (be ∅) :➜leftmost (be t)})

      (tape-length-is-one tp00 {:➜∅ (be t) :➜t (be ∅)})
      (tape-length-is-one tp1 {:➜∅ (be ∅) :➜t (be t)})
      (tape-length-is-one tp2 {:➜∅ (be t) :➜t (be ∅)})

      (tape-length-is-two tp00 {:➜∅ (be t) :➜t (be ∅)})
      (tape-length-is-two tp1 {:➜∅ (be t) :➜t (be ∅)})
      (tape-length-is-two tp2 {:➜∅ (be ∅) :➜t (be t)})
      (tape-length-is-two tp3 {:➜∅ (be t) :➜t (be ∅)})

      )))
(test-hook test-tape-0)
