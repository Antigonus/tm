
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
        (➜bound-right (λ()(error 'step-from-bound-right)))
        &allow-other-keys
        )
      ➜
      (let(
            (bound-left (cons-list tape))
            )
        (if
          (cdr bound-left)
          [➜ok (cadr bound-left)]
          [➜bound-right]
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
        (➜bound-right (be ∅))
        &allow-other-keys
        )
      ➜
      (let(
            (bound-left (cons-list tape))
            )
        (if
          (cdr bound-left)
          (progn
            (setf (cadr bound-left) instance)
            [➜ok]
            )
          [➜bound-right]
          ))))
