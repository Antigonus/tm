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
         (tp0 (mk 'list-tape ∅))
         (tp00 (mk 'list-tape tp0))
         (tp1 (mk 'list-tape #('a)))
         (tp2 (mk 'list-tape #('a 'b)))
         (tp3 (mk 'list-tape #('a 'b 'c)))
         )
    (∧
      (typep tp0 'tape-empty)
      (typep tp00 'tape-empty)

      (e-s*r tp0 {:➜ok (be ∅) :➜empty (be t)})
      (e-s*r tp1 {:➜ok (be t) :➜empty (be ∅)})

      (e-s*sr tp00 {:➜ok (be ∅) :➜rightmost (be ∅) :➜empty (be t)})
      (e-s*sr tp1  {:➜ok (be ∅) :➜rightmost (be t) :➜empty (be ∅)})
      (e-s*sr tp2  {:➜ok (be t) :➜rightmost (be ∅) :➜empty (be ∅)})

      (e-s*w tp0 12 {:➜ok (be ∅) :➜empty (be t)})
      (e-s*w tp1 13 {:➜ok (be t) :➜empty (be ∅)})

      (e-s*sw tp0 14 {:➜ok (be ∅) :➜rightmost (be ∅) :➜empty (be t)})
      (e-s*sw tp1 15 {:➜ok (be ∅) :➜rightmost (be t) :➜empty (be ∅)})
      (e-s*sw tp2 16 {:➜ok (be t) :➜rightmost (be ∅) :➜empty (be ∅)})

      (es*r tp0 {:➜ok (be ∅) :➜empty (be t)})
      (es*-sr tp00 {:➜ok (be ∅) :➜empty (be t)})
      (es*w tp0 16 {:➜ok (be ∅) :➜empty (be t)})
      (es*-sw tp00 17 {:➜ok (be ∅) :➜empty (be t)})

      (leftmost tp00 {:➜ok (be ∅) :➜empty (be t)})
      (leftmost tp1 {:➜ok (be t) :➜empty (be ∅)})

      (epd<tape> tp0 {:➜ok (be ∅) :➜rightmost (be t)})
      (es*-sd<tape> tp00 {:➜ok (be ∅) :➜leftmost (be t)})

      (tape-length-is-one tp00 {:➜∅ (be t) :➜t (be ∅)})
      (tape-length-is-one tp1 {:➜∅ (be ∅) :➜t (be t)})
      (tape-length-is-one tp2 {:➜∅ (be t) :➜t (be ∅)})

      (tape-length-is-two tp00 {:➜∅ (be t) :➜t (be ∅)})
      (tape-length-is-two tp1 {:➜∅ (be t) :➜t (be ∅)})
      (tape-length-is-two tp2 {:➜∅ (be ∅) :➜t (be t)})
      (tape-length-is-two tp3 {:➜∅ (be t) :➜t (be ∅)})

      )))
(test-hook test-tape-0)
