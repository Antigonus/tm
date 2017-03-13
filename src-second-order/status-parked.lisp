#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

(defmacro def-parked-1 (f &rest args)
  `(defun-typed ,f ((tm status-parked) ,@args &optional ➜)
     (declare (ignore ,@args))
     (destructuring-bind
       (
         &key
         (➜parked #'access-through-parked-head)
         &allow-other-keys
         )
       ➜
       [➜parked]
       ))
  )


;;--------------------------------------------------------------------------------
;; status-tm definitions
;;
  (defun-typed park ((tm status-parked) &optional ➜)
     (declare (ignore tm))
     (destructuring-bind
       (
         &key
         (➜ok (be t))
         &allow-other-keys
         )
       ➜
       [➜ok]
       ))


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (def-parked-1 r)

  (defun-typed esr ((tm status-parked) &optional ➜) (r◧ tm ➜))

  (def-parked-1 w instance)

  (defun-typed esw ((tm status-parked) instance &optional ➜) (w◧ tm ➜))

  (defun-typed c◧ ((tm status-parked) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (to-active tm)
      [➜ok]
      ))

  (defun-typed s ((tm status-parked) &optional ➜) (c◧ tm ➜))

  (defun-typed a ((tm status-parked) instance &optional ➜) (a◧ tm instance ➜))

  (defun-typed on-leftmost ((tm status-parked) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜∅        (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun-typed on-rightmost ((tm status-parked) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜∅        (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

;;--------------------------------------------------------------------------------
;;tm-generic
;;
  (defun-typed c◨ ((tm status-parked) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (c◨ (base tm)
        {
          :➜ok (λ()(to-active tm)[➜ok])
          }
        )))
  
  (defun-typed as ((tm status-parked) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (a◧ (base tm) instance
        {
          :➜ok (λ()(c◧ tm ➜))
          :➜no-alloc ➜no-alloc
          })
      ))

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm status-parked) instance &optional ➜) (a◧ (base tm) instance ➜))
  (defun-typed d ((tm status-parked) &optional spill ➜) (d◧ (base tm) spill ➜))
  (defun-typed d◧ ((tm status-parked) &optional spill ➜) (d◧ (base tm) spill ➜))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;

  (defun-typed heads-on-same-cell 
    (
      (tm0 status-parked)
      (tm1 status-parked)
      &optional ➜
      )
    (entangled tm0 tm1 ➜)
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 status-parked)
      (tm1 tape-machine) ; status can not empty (or all machines in group would be empty)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun-typed heads-on-same-cell
    (
      (tm0 tape-machine)  ; status can not empty (or all machines in group would be empty)
      (tm1 status-parked)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))


;;--------------------------------------------------------------------------------
;; nd-tm-generic
;;

  ;; tm0 and tm1 are entangled, thus are of the same type
  ;; tm0 empty <=> tm1 empty
  ;; tm0 abandoned <=> tm1 abandoned
  ;; tm0 parked, tm1 can be active, vice versa
  (defun-typed s≠ 
    (
      (tm0 status-parked)
      (tm1 status-parked)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (entangled tm0 tm1
        {
          :➜t ➜rightmost
          :➜∅ (λ()(c◧ tm0 ➜))
          }
        )))

  (defun-typed s≠ 
    (
      (tm0 status-parked)
      (tm1 status-active)
      &optional ➜
      )
    (declare (ignore tm1))
    (c◧ tm0 ➜)
    )

  (defun-typed a◨ ((tm status-parked) instance &optional ➜) (a◨ (base tm) instance ➜))
