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
          (tma (mk 'list-nd-tm)) ; can't make stateful machine yet ...
          (tmb (mk 'list-nd-tm :mount {1}))
          (tmc (mk 'list-nd-tm :mount {1 2}))
          (tmd (mk 'list-nd-tm :mount {1 2 3}))
          )
      (sync (mount {tma tmb tmc tmd})
        #'is-active
        (λ()
            (∧
              (singleton tma)
              (singleton tmb)
              (not (doubleton tmb))
              (not (singleton tmc))
              (not (tripleton tmc))
              (doubleton tmc)
              (not (singleton tmd))
              (not (doubleton tmd))
              (tripleton tmd)
              ))
        (λ(retry tms)
          (declare (ignore tms))
          (as tma 0)
          (funcall retry)
          ))))


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


