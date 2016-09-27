#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defmethod a◧
    (
      (tm list-solo-tm)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (declare (ignore cont-no-alloc))
    (setf (tape tm) (cons object (tape tm)))
    (funcall cont-ok)
    )

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  (defmethod d 
    (
      (tm list-solo-tm)
      &optional
      spill 
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-no-alloc #'alloc-fail)
      &rest ⋯
      )
    (declare (ignore ⋯))
    (if
      (cdr (HA tm))
      (let*(
             (dealloc-cell (cdr (HA tm)))
             (spill-object (car dealloc-cell))
             )
        (if spill
          (as spill spill-object
            (λ()
              (rplacd (HA tm) (cdr dealloc-cell))
              (funcall cont-ok spill-object)
              )
            cont-no-alloc
            )
          (progn
            (rplacd (HA tm) (cdr dealloc-cell))
            (funcall cont-ok spill-object)
            )))

      (funcall cont-rightmost)
      ))

  ;; We depend on the fact that the head must be on some cell.
  ;; It follows that if there is only one cell, it is leftmost, and the head is on it.
  ;; As we refuse to delete the cell with the head on it, the last cell can not be deleted.
  ;; Consequently we can never have a cont-rightmost.
  (defmethod d◧
    (
      (tm list-solo-tm)
      &optional
      spill 
      (cont-ok #'echo)
      (cont-no-alloc #'alloc-fail)
      (cont-collision (λ()(error 'dealloc-collision)))
      )
    (if
      (eq (HA tm) (tape tm))
      (funcall cont-collision)
      (let*(
             (dealloc-cell (tape tm))
             (spill-object (car dealloc-cell))
             )
        (when spill
          (as spill spill-object
            (λ()
              (setf (tape tm) (cdr dealloc-cell))
              (funcall cont-ok spill-object)
              )
            cont-no-alloc
            )))))
