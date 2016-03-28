#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

  (defun test-Δ-0 ()
    (let(
          (tm (tm-mk 'tm-list {1 2 3}))
          )
      (Δ "ssw" tm 7)
      (equal
        (unmount tm)
        {1 2 7}
        )))

  (test-hook test-Δ-0)

