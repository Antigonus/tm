#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defmethod a&h◨ 
    (
      (tm list-tm)
      instance
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (declare (ignore cont-no-alloc))
    (rplacd (head tm) (cons instance ∅))
    (funcall cont-ok)
    )

  (defmethod as&h◨ 
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
    (funcall cont-ok)
    )

