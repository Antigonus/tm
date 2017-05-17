#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun-typed epa ((tm list-solo-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (tape tm) (cons instance (tape tm)))
      [➜ok]
      ))

  (defun-typed update-tape-after-epa ((tm list-solo-tm) (tm-ref list-solo-tm))
    (setf (tape tm) (tape tm-ref))
    )
  (defun-typed update-tape-after-epd ((tm list-solo-tm) (tm-ref list-solo-tm))
    (setf (tape tm) (tape tm-ref))
    )


;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  (defun-typed d ((tm list-solo-tm) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo) ; echoes the instance from the deleted cell
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (if
        (cdr (head tm))
        (let*(
               (dealloc-cell (cdr (head tm)))
               (spill-instance (car dealloc-cell))
               )
          (labels
            (
              (spill-circuit ()
                (if spill
                  (as spill spill-instance
                    {
                      :➜ok #'delete-cell
                      :➜no-alloc ➜no-alloc
                      }
                    )
                  (delete-cell)
                  ))
              (delete-cell()
                (rplacd (head tm) (cdr dealloc-cell))
                [➜ok spill-instance]
                ))
            (spill-circuit)
            ))
        [➜rightmost]
        )))


  ;; We depend on the fact that the head must be on some cell.
  ;; It follows that if there is only one cell, it is leftmost, and the head is on it.
  ;; As we refuse to delete the cell with the head on it, the last cell can not be deleted.
  ;; Consequently we can never have a ➜rightmost.
  (defun-typed epd ((tm list-solo-tm) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜no-alloc #'alloc-fail)
        (➜collision (λ()(error 'dealloc-collision)))
        &allow-other-keys
        )
      ➜
      (if
        (eq (head tm) (tape tm))
        [➜collision]
        (let*(
              (dealloc-cell (tape tm))
              (spill-instance (car dealloc-cell))
              )
          (labels
            (
              (spill-circuit ()
                (if spill
                  (as spill spill-instance
                    {
                      :➜ok #'delete-cell
                      :➜no-alloc ➜no-alloc
                      }
                    )
                  (delete-cell)
                  ))
              (delete-cell ()
                (setf (tape tm) (cdr dealloc-cell))
                [➜ok spill-instance]
                ))
            (spill-circuit)
            )))))
