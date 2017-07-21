#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-tape-0 ()
  (let(
        (x0 (mk 'tm ∅))
       )
    (∧
      (typep x0 'tape-machine-empty)
      (as x0 1)
      (on-left-bound x0)
      (on-right-bound x0)
      (as x0 2)
      (¬ (on-left-bound x0))
      (on-right-bound x0)
      (as x0 3)
      (¬ (on-left-bound x0))
      (on-right-bound x0)
      (= (◧r x0) 1)
      (= (◧sr x0) 2)
      (= (◧snr x0 2) 3)
      (= (◧snr x0 3 {:➜right-bound (λ(cell n)(declare (ignore cell))n)}) 1)
      )))     
(test-hook test-tape-0)


(defun test-tape-1 ()
  (let(
        (x0 (mk 'tm {10 20 30}))
       )
    (∧
      (= (◧r x0) 10)
      (◧r x0 {:➜ok (λ(x)(= x 10)) :➜empty (be ∅) :➜right-bound (be ∅)})
      (= (◧sr x0) 20)
      (◧sr x0 {:➜ok (λ(x)(= x 20)) :➜empty (be ∅) :➜right-bound (be ∅)})
      (= (◧snr x0 2) 30)
      (◧snr x0 2 {:➜ok (λ(x)(= x 30)) :➜empty (be ∅) :➜right-bound (be ∅)})
      (◧snr x0 3 {:➜ok (be ∅) :➜empty (be ∅) :➜right-bound (be t)})
      )))
(test-hook test-tape-1)

(defun test-tape-2 ()
  (let(
        (x0 (mk 'tm ∅ {:type 'tape-bilist}))
        (x1 (mk 'tm {1 2 3} {:type 'tape-bilist}))
       )
    (∧
      (= (◧w x0 5 {:➜ok (be 21) :➜empty (be 27)}) 27)
      (◧w x1 10)
      (◧sw x1 20)
      (◧snw x1 2 30)
      (= (◧snw x1 3 40 {:➜ok (be 31) :➜empty (be 33) :➜right-bound (be 37)}) 37)
      (= (◧r x1) 10)
      (= (◧sr x1) 20)
      (= (◧snr x1 2) 30)
      )))
(test-hook test-tape-2)

