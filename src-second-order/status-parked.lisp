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

  (defun-typed esr ((tm status-parked) &optional ➜) (ec◧r tm ➜))

  (def-parked-1 w instance)

  (defun-typed esw ((tm status-parked) instance &optional ➜) (ec◧w tm ➜))

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
  (defun-typed -s ((tm status-parked) &optional ➜) (c◨ tm ➜))

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
  
;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm status-parked) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (prins (print "a◧ status-parked-active"))
      (a◧ (base tm) instance
        {
          :➜ok (λ()
                 (c◧ (base tm)) ; for a parked machine we always leave the base head on leftmost
                 (incf (address-rightmost tm))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))


  (defun-typed d◧ ((tm status-parked) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜no-alloc #'alloc-fail)
        ;; no collision continuation possible because the head is parked
        &allow-other-keys
        )
      ➜
      (prins (print "d◧ status-parked"))

        (if
          (= (address-rightmost tm) 0)

          (let(
                (instance (r (base tm)))
                )
            (labels(
                     (faux-delete-the-only-cell ()
                       (w (base tm) ∅)
                       (to-empty tm)
                       [➜ok instance]
                       )
                     )
              (if spill
                (as spill instance
                  {
                    :➜ok #'faux-delete-the-only-cell
                    :➜no-alloc ➜no-alloc
                    })
                (faux-delete-the-only-cell)
                )))

          (progn
            (s (base tm) ; need to get the base head off of ◧ where we leave it when parked
              {:➜rightmost #'cant-happen} ; we know that address-rightmost is not 0
              )
            (d◧ (base tm) spill
              {
                :➜ok (λ(instance)
                       (decf (address tm))
                       (decf (address-rightmost tm))
                       [➜ok instance]
                       )
                :➜collision #'cant-happen ; the prior #'s call just moved the head out of the way
                (o (remove-key-pairs ➜ {:➜ok :➜collision}))
                })
            ))))

  (defun-typed d ((tm status-parked) &optional spill ➜) (d◧ tm spill ➜))


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

