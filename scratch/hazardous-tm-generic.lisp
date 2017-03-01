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
      &optional ➜
      )
    (call-next-method tm instance ➜) ; calls the solo version
    (∀-entanglements-update-tape tm)
    )

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  (defun-typed d
    (
      (tm ea-tape-machine)
      &optional spill ➜
      )
    (destructuring-bind
      (&key
        (➜collision (λ()(error 'dealloc-collision)))
        &allow-other-keys
        )
      ➜
      (∃-collision-right-neighbor 
        tm
        ➜collision 
        (λ()(call-next-method tm spill ➜))
        )))
      
  (defun-typed d◧
    (
      (tm ea-tape-machine)
      &optional spill ➜
      )
    (destructuring-bind
      (&key
        (➜collision (λ()(error 'dealloc-collision)))
        &allow-other-keys
        )
      ➜
      (∃-collision◧ tm
        ➜collision
        (λ()
          (call-next-method tm spill ➜)
          (∀-entanglements-update-tape tm)
          ))
      ))

    

