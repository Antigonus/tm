#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions are common to all states where they are not directly 
overridden.  

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; new function classes for status
;;
  (def-function-class hpd* (tm &optional spill ➜)
    (:documentation
      "Deallocates the tape"
      ))

;;--------------------------------------------------------------------------------
;; abandoned
;;
  (defun-typed d* ((tm abandoned) &optional spill ➜)
    (declare (ignore tm spill ➜))
    (operation-on-abandoned)
    )

  (defun-typed filter ((tm abandoned) (spill tape-machine) pred &optional ➜)
    (declare (ignore tm spill pred ➜))
    (operation-on-abandoned)
    )

  (defun-typed filter ((tm tape-machine) (spill abandoned) pred &optional ➜)
    (declare (ignore tm spill pred ➜))
    (operation-on-abandoned)
    )

;;--------------------------------------------------------------------------------
;; empty
;;
  (defun-typed d* ((tm empty) &optional spill ➜)
    (declare (ignore spill))
    (destructuring-bind
      (&key
        (➜rightmost (be t))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  (defun-typed filter ((tm empty) (spill tape-machine) pred &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      [➜ok]
      ))
 
;;--------------------------------------------------------------------------------
;; parked
;;
  (defun-typed d* ((tm parked) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜rightmost (be t))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (d* (base tm) spill
        {
          :➜rightmost (λ()
                        (w (base tm) ∅)
                        (to-empty tm)
                        [➜rightmost]
                        )
          :➜no-alloc ➜no-alloc
          })))

;;--------------------------------------------------------------------------------
;; active
;;
  (defun-typed d* ((tm active) &optional spill ➜)
    (d* (base tm) spill ➜)
    )

