#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

call-next method falls back to generic implementations for nd-tape-machine

these are 


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; add a new leftmost
  (defun-typed a◧
    (
      (tm ea-tape-machine)
      instance
      &optional
      (cont-ok  (be t))
      (cont-no-alloc #'alloc-fail)
      &rest ⋯
      )
    (declare (ignore ⋯))
    (call-next-method tm instance cont-ok cont-no-alloc) ; calls the solo version
    (∀-entanglements-update-tape tm)
    )

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  (defun-typed d
    (
      (tm ea-tape-machine)
      &optional 
      spill 
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-no-alloc #'alloc-fail)
      (cont-collision (λ()(error 'dealloc-collision)))
      &rest ⋯
      )
    (declare (ignore ⋯))
    (∃-collision-right-neighbor 
      tm
      cont-collision 
      (λ()(call-next-method tm spill cont-ok cont-rightmost cont-no-alloc))
      ))
      
  (defun-typed d◧
    (
      (tm ea-tape-machine)
      &optional 
      spill 
      (cont-ok #'echo)
      (cont-collision (λ()(error 'dealloc-collision)))
      (cont-no-alloc #'alloc-fail)
      &rest ⋯
      )
    (declare (ignore ⋯))
    (∃-collision◧ tm
      cont-collision
      (λ()
        (call-next-method tm spill cont-ok cont-collision cont-no-alloc)
        (∀-entanglements-update-tape tm)
        )))

    

