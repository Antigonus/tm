#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; status-tm definitions
;;
  (defun-typed p ((tm parked) &optional ➜)
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

  (defun-typed esr ((tm parked) &optional ➜) (◧r tm ➜))

  (def-parked-1 w instance)

  (defun-typed esw ((tm parked) instance &optional ➜) (◧w tm ➜))

  (defun-typed -s* ((tm parked) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (to-active tm)
      [➜ok]
      ))

  (defun-typed s ((tm parked) &optional ➜) (-s* tm ➜))
  (defun-typed -s ((tm parked) &optional ➜) (s* tm ➜))

  (defun-typed a ((tm parked) instance &optional ➜) (epa tm instance ➜))

  (defun-typed on-leftmost ((tm parked) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜∅        (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun-typed on-rightmost ((tm parked) &optional ➜)
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
  (defun-typed s* ((tm parked) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (s* (base tm)
        {
          :➜ok (λ()(to-active tm)[➜ok])
          }
        )))
  
;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed epa ((tm parked) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      ;; (prins (print "epa parked-active"))
      (epa (base tm) instance
        {
          :➜ok (λ()
                 (-s* (base tm)) ; for a parked machine we always leave the base head on leftmost
                 (incf (address-rightmost tm))
                 [➜ok]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))


  (defun-typed epd ((tm parked) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜no-alloc #'alloc-fail)
        ;; no collision continuation possible because the head is parked
        &allow-other-keys
        )
      ➜
      ;; (prins (print "epd parked"))
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
          (s (base tm) ; need to get the base head off of -s* where we leave it when parked
            {:➜rightmost #'cant-happen} ; we know that address-rightmost is not 0
            )
          (epd (base tm) spill
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

  (defun-typed d ((tm parked) &optional spill ➜) (epd tm spill ➜))

  (defun-typed d. ((tm parked) &optional spill ➜)
    (declare (ignore tm spill))
    (destructuring-bind
      (&key
        (➜fail (λ()(error 'dealloc-parked)))
        &allow-other-keys
        )
      ➜
      [➜fail]
      ))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed heads-on-same-cell 
    (
      (tm0 parked)
      (tm1 parked)
      &optional ➜
      )
    (entangled tm0 tm1 ➜)
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 parked)
      (tm1 status-tm)
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
      (tm0 status-tm) 
      (tm1 parked)
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
      (tm0 parked)
      (tm1 parked)
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
          :➜∅ (λ()(-s* tm0 ➜))
          }
        )))

  (defun-typed s≠ 
    (
      (tm0 parked)
      (tm1 active)
      &optional ➜
      )
    (declare (ignore tm1))
    (-s* tm0 ➜)
    )

