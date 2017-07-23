#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

seems this should wait until after we have a tape machine ..

max-address
max-address-compare

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; max-address
;;
  (def-function-class max-address (tape &optional ➜))
  (defun-typed max-address ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      )
  (defun-typed max-address ((tape solitary) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        )
      ➜
      [➜ok 0]
      ))
  (defun-typed max-address ((tape tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        )
      ➜
      (let(
            (n 0)
            (cell tape)
            )
        (⟳(λ(➜again)
            (neighbor cell
              {
                :➜ok
                (λ(n-cell)
                  (setf cell n-cell)
                  (incf n)
                  [➜again]
                  )
                :➜right-bound
                (λ(cell n)
                  (declare (ignore cell n)) 
                  [➜ok n]
                  )})))
        )))

;;--------------------------------------------------------------------------------
;; max-address compares
;;
  (def-function-class max-address-compare(tape &optional ➜))
  (defun-typed max-address ((tape tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      )
  (defun-typed max-address ((tape solitary) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        )
      ➜
      [➜ok 0]
      ))
  (defun-typed max-address ((tape tape-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        )
      ➜
      (let(
            (n 0)
            (cell tape)
            )
        (⟳(λ(➜again)
            (neighbor cell
              {
                :➜ok
                (λ(n-cell)
                  (setf cell n-cell)
                  (incf n)
                  [➜again]
                  )
                :➜right-bound
                (λ(cell n)
                  (declare (ignore cell n)) 
                  [➜ok n]
                  )})))
        )))
