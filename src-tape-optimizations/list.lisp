
#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Need to add in the no-alloc continuations

    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜bad (λ()(error 'bad-init-value)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜

|#

(in-package #:tm)


these list specific versions passed tests, I just don't want the overhead of
more code to maintain right now


  (defun-typed e-s*r ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (car (cons-list tape))]
      ))

  (defun-typed e-s*sr ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (let(
            (leftmost (cons-list tape))
            )
        (if
          (cdr leftmost)
          [➜ok (cadr leftmost)]
          [➜rightmost]
          ))))

  (defun-typed e-s*w ((tape tape-list-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (car (cons-list tape)) instance)
      [➜ok]
      ))

  (defun-typed e-s*sw ((tape tape-list-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (let(
            (leftmost (cons-list tape))
            )
        (if
          (cdr leftmost)
          (progn
            (setf (cadr leftmost) instance)
            [➜ok]
            )
          [➜rightmost]
          ))))
