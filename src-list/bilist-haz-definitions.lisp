#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; cell deallocation
;;

  #|
     a hazardous operation -- tm is no longer valid after this call

     For second level machine tm will be abandoned in the call. 

     If a programmer wants to keep tm valid, then step tm left 
     or right and call d or -d, respectively.

     One might read this code and surmise that tm can be used
     after the call, however, another implemementation might
     behave differently. 
  |#
  (defun-typed d. ((tm bilist-haz-tm) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo) ; echoes the instance from the deleted cell
        (➜fail (λ()(error 'dealloc-last)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (let*(
             (instance (r tm))
             (node (head tm))
             (left-neighbor (binode-left-neighbor node))
             (right-neighbor (binode-right-neighbor node))
             )
        (labels(
                 (stitch ()
                   (if left-neighbor
                     (setf (binode-right-neighbor left-neighbor) right-neighbor)
                     (setf (tape tm) right-neighbor)
                     )
                   (when right-neighbor
                     (setf (binode-left-neighbor right-neighbor) left-neighbor)
                     )
                   [➜ok instance]
                   )
                 )
          (if (∧ (¬ left-neighbor) (¬ right-neighbor))
            [➜fail] ; first level tape machines must have at least one cell
            (if spill
              (a tm instance
                {
                  :➜no-alloc ➜no-alloc
                  :➜ok #'stitch
                  })
              (stitch)
              ))))))



