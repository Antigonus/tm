#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions are derived just from the primitives, so there is no functional need for a
new tape machine implementation to specialize them.


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cueing
;;  
  (def-function-class ◨ (tm &optional ➜)
    (:documentation
      "Cue tm's head to the rightmost cell."
      ))

  (defun-typed ◨ ((tm tape-machine) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (labels(
               (work() (s tm {:➜ok #'work :➜rightmost ➜ok}))
               )
        (work)
        )))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or though a fill machine.
;;
  (def-function-class as (tm instance &optional ➜)
    (:documentation
      "Like #'a, but tm is stepped to the new cell
      "))

  (defun-typed as
    (
      (tm tape-machine)
      instance
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (a tm instance
        {
          :➜ok (λ()(s tm {:➜ok ➜ok :➜rightmost #'cant-happen}))
          :➜no-alloc ➜no-alloc 
          })
      ))

  (def-function-class a&◨ (tm instance &optional ➜)
    (:documentation
      "#'a with a contract that the head is on rightmost.
      "))

  ;; specializations might make better use of the contract
  (defun-typed a&◨ 
    (
      (tm tape-machine)
      instance
      &optional ➜
      )
      (a tm instance ➜)
      )

  (def-function-class as&◨ (tm instance &optional ➜)
    (:documentation
      "#'as with a contract that the head is on rightmost.
      "))

   ;; specializations might make better use of the contract
  (defun-typed as&◨ 
    (
      (tm tape-machine)
      instance
      &optional ➜
      )
    (as tm instance ➜)
    )

    
