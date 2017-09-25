#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  pred takes three arguments, a tape machine, a false continuation, and a true
  continuation.  false is normally 0, and true 1,  so the false clause come first.
  Here we only support two value logic.

  Optionally provide a step function.

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; Existence
;;

  (def-function-class ∃ (tm pred &optional ➜))
  (defun-typed ∃ ((tm tm-abandoned) pred &optional ➜)
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed ∃ ((tm tm-empty) (pred function) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
  (defun-typed ∃ ((tm tm-parked) (pred function) &optional ➜)
    (destructuring-bind
      (&key
        (first #'s) ; must unpark the head on a parked machine
        &allow-other-keys
        )
      ➜
      [first tm ; unpark
        {
          :➜ok (λ()(∃ tm pred ➜))
          (o ➜)
          }]))
  (defun-typed ∃ ((tm tm-active) (pred function) &optional ➜)
    (destructuring-bind
      (&key
        (step #'s) ; steps the head
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (⟳(λ(again)
          [pred tm (λ()[step tm {:➜ok again :➜bound ➜∅}]) ➜t]
          ))))
      
;;--------------------------------------------------------------------------------
;; Universal
;;
  (def-function-class ∀ (tm pred &optional ➜))
  (defun-typed ∀ ((tm tm-abandoned) pred &optional ➜) 
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed ∀ ((tm tm-empty-parked-active) pred &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
    (∃ 
      tm
      (λ(tm c∅ ct)[pred tm ct c∅])
      {:➜∅ ➜t :➜t ➜∅ (o ➜)}
      )))

  ;; parks the head before calling quantifier
  (def-function-class p∃ (tm pred &optional ➜))
  (defun-typed p∃ ((tm tm-abandoned) pred &optional ➜) 
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed p∃ ((tm tm-empty-parked-active) pred &optional ➜)
    (p tm
      {:➜ok (∃ tm pred ➜)}
      (o ➜)
      ))
  (def-function-class p∀ (tm pred &optional ➜))
  (defun-typed p∀ ((tm tm-abandoned) pred &optional ➜) 
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed p∀ ((tm tm-empty-parked-active) pred &optional ➜)
    (p tm
      {:➜ok (∀ tm pred ➜) }
      (o ➜)
      ))
