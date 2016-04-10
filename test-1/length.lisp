#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package #:tm)

;;--------------------------------------------------------------------------------
;; length.lisp
;;
  (defun test-ton-0 ()
    (let(
          (a (tm-mk 'tm-list))
          (b (mount {1}))
          (c (mount {1 2}))
          (d (mount {1 2 3}))
          )
      (synch #'not-parked {a}
        (be 'ready-path)
        (λ(retry tms)
          (declare (ignore retry))
          (destructuring-bind (aa) tms
            (∧
              (parked aa)
              (singleton b)
              (not (doubleton b))
              (not (singleton c))
              (not (tripleton c))
              (doubleton c)
              (not (singleton d))
              (not (doubleton d))
              (tripleton d)
              ))))))
  (test-hook test-ton-0)

  (defun test-length≥-0 () 
    (let(
          (tm (mount [a b c]))
          )
    (and
      (length≥ tm 2)
      (length≥ tm 3)
      (not (length≥ tm 4))
      )))
  (test-hook test-length≥-0)

  (defun test-length=-0 () 
    (let(
          (tm (mount [a b c]))
          )
      (and
        (not (length= tm 2))
        (length= tm 3)
        (not (length= tm 4))
        )))
  (test-hook test-length=-0)


