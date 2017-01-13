#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun-typed a&h◨ 
    (
      (tm list-tm)
      instance
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (declare (ignore cont-no-alloc))
    (rplacd (head tm) (cons instance ∅))
    [cont-ok]
    )

  (defun-typed as&h◨ 
    (
      (tm list-tm)
      instance
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (declare (ignore cont-no-alloc))
    (rplacd (head tm) (cons instance ∅))
    (setf (head tm) (cdr (head tm)))
    [cont-ok]
    )

