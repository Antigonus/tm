#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun-typed epa ((tm bilist-solo-tm) instance &optional ➜)
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

   ;; update-tape-after-epa and update-tape-after-epd inherited from list-solo-tm


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

  (defun-typed -d ((tm bilist-solo-tm) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo) ; echoes the instance from the deleted cell
        (➜leftmost (λ()(error 'left-dealloc-on-leftmost)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (let(
            (indicated-cell (head tm))
            )
        (if
          (¬ (binode-left-neighbor indicated-cell))
          [➜leftmost]
          (let*(
                 (dealloc-cell (binode-left-neighbor indicated-cell))
                 (instance (binode-instance dealloc-cell))
                 )
            (labels
              (
                (spill-circuit ()
                  (if spill
                    (as spill instance
                      {
                        :➜ok #'delete-cell
                        :➜no-alloc ➜no-alloc
                        }
                      )
                    (delete-cell)
                    ))
                
                (delete-cell()
                  (let(
                        (new-left-neighbor (binode-left-neighbor dealloc-cell)) ; might be nil
                        )
                    (setf
                      (binode-left-neighbor indicated-cell) 
                      new-left-neighbor
                      )
                    (when new-left-neighbor
                      (setf
                        (binode-right-neighbor new-left-neighbor) 
                        indicated-cell
                        ))
                    [➜ok instance]
                    ))
                )
              (spill-circuit)
              ))
          ))))



  ;; We depend on the fact that the head must be on some cell.
  ;; It follows that if there is only one cell, it is leftmost, and the head is on it.
  ;; As we refuse to delete the cell with the head on it, the last cell can not be deleted.
  ;; Consequently we can never have a ➜rightmost.
  (defun-typed epd ((tm bilist-solo-tm) &optional spill ➜)
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
