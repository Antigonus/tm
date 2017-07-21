
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


  (defun-typed ◧r ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (car (cons-list tape))]
      ))

  (defun-typed ◧sr ((tape tape-list-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜right-bound (λ()(error 'step-from-right-bound)))
        &allow-other-keys
        )
      ➜
      (let(
            (left-bound (cons-list tape))
            )
        (if
          (cdr left-bound)
          [➜ok (cadr left-bound)]
          [➜right-bound]
          ))))

  (defun-typed ◧w ((tape tape-list-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (car (cons-list tape)) instance)
      [➜ok]
      ))

  (defun-typed ◧sw ((tape tape-list-active) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜right-bound (be ∅))
        &allow-other-keys
        )
      ➜
      (let(
            (left-bound (cons-list tape))
            )
        (if
          (cdr left-bound)
          (progn
            (setf (cadr left-bound) instance)
            [➜ok]
            )
          [➜right-bound]
          ))))
