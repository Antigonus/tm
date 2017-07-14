#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-heads-on-same-cell-0 ()
  (let(
        (tm0 (mk 'list-nd-tm {:tape {7 2 -3}}))
        )
    (with-entangled tm0
      (λ(tm1)
        (∧
          (s tm0)
          (¬ (heads-on-same-cell tm0 tm1))
          (s tm1)
          (heads-on-same-cell tm0 tm1)
          (s tm1)
          (¬ (heads-on-same-cell tm0 tm1))
          )))))
(test-hook test-heads-on-same-cell-0)
