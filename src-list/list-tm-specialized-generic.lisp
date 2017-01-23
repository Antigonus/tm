#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun-typed a&h◨ (tm instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (rplacd (head tm) (cons instance ∅))
      [➜ok]
      ))

  (defun-typed as&h◨ (tm instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (rplacd (head tm) (cons instance ∅))
      (setf (head tm) (cdr (head tm)))
      [➜ok]
      ))
