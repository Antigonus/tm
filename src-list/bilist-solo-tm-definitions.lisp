#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun-typed a◧ ((tm bilist-solo-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (let(
            (node-0 (make-binode))
            (node-1 (tape tm))
            )
        (setf (binode-left-neighbor node-0) ∅)
        (setf (binode-right-neighbor node-0) node-1)
        (setf (binode-left-neighbor node-1) node-0)
        (setf (tape tm) node-0)
        [➜ok]
        )))

   ;; update-tape-after-a◧ and update-tape-after-d◧ inherited from list-solo-tm


;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  (defun-typed d ((tm bilist-solo-tm) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo) ; echoes the instance from the deleted cell
        (➜rightmost (λ()(error 'dealloc-on-rightmost)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (if
        (binode-right-neighbor (head tm))
        (let*(
               (position-cell (head tm))
               (dealloc-cell (binode-right-neighbor position-cell))
               (spill-instance (binode-instance dealloc-cell))
               (new-right-neighbor (binode-right-neighbor dealloc-cell))
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
                (setf
                  (binode-right-neighbor position-cell) 
                  new-right-neighbor
                  )
                (setf
                  (binode-left-neighbor new-right-neighbor) 
                  position-cell
                  )
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
  (defun-typed d◧ ((tm bilist-solo-tm) &optional spill ➜)
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
               (spill-instance (binode-instance dealloc-cell))
               (new-leftmost (binode-right-neighbor dealloc-cell))
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
                (setf (binode-left-neighbor new-leftmost) ∅)
                (setf (tape tm) new-leftmost)
                [➜ok spill-instance]
                ))
            (spill-circuit)
            )))))
