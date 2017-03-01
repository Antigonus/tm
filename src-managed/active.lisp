#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

We never make  empty machines, rather managed machines are change classed
to this type after a request to delete the last cell from the tape
belonging to a machine that has a parked head.

Wonder if we should inform the manager of operation on empty, then
the programmer would have the opportunity to provide a recovery mechanism.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (def-type empty-mtm (managed-tm))

;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defmacro def-empty-1 (f &rest args)
    `(defun-typed f ((tm empty-mtm) ,@args &optional ➜)
       (declare (ignore ,@args))
       (destructuring-bind
         (
           &key
           (➜empty #'use-of-empty)
           &allow-other-keys
           )
         ➜
         [➜empty]
         ))
    )

  (def-empty-1 r)

  (defun-typed esr ((tm empty-mtm) &optional ➜)
    (declare (ignore tm))
    (destructing-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))


  (def-empty-1 w instance)

  (defun-typed esw ((tm empty-mtm) instance &optional ➜)
    (declare (ignore tm))
    (destructing-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  (def-empty-1 cue-leftmost)

  (defun-typed s ((tm empty-mtm) &optional ➜)
    (declare (ignore tm))
    (destructing-bind
      (
        &key
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      [➜rightmost]
      ))

  (defun-typed a ((tm empty-mtm) instance &optional ➜)
    (case (status tm)
      ('active (a (base tm) instance ➜))
      ('parked (a◧ (base tm) instance ➜))
      ('empty  (a&empty tm ➜)) ; will change status to parked
      ('abandoned (operation-on-abandoned))
      (otherwise  [cant-happen])
      ))

  (defun-typed on-leftmost ((tm empty-mtm) &optional ➜)
    (case (status tm)
      ('active (on-leftmost (base tm) ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise
        (destructuring-bind
          (
            &key
            (➜∅        (be ∅))
            &allow-other-keys
            )
          ➜
          (case (status tm)
            (('empty 'parked) [➜∅])
            (otherwise  [cant-happen])
            )))))

  (defun-typed on-rightmost ((tm empty-mtm) &optional ➜)
    (case (status tm) 
      ('active (on-rightmost (base tm) ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise
        (destructuring-bind
          (
            &key
            (➜∅        (be ∅))
            &allow-other-keys
            )
          ➜
          (case (status tm)
            (('empty 'parked) [➜∅])
            (otherwise  [cant-happen])
            )))))

;;--------------------------------------------------------------------------------
;;tm-generic
;;
  (defun-typed cue-rightmost ((tm empty-mtm) &optional ➜)
    (case (status tm)
      ('active (cue-rightmost (base tm) ➜))
      ('parked
        (setf (status tm) 'active)
        (cue-rightmost (base tm) ➜)
        )
      ('abandoned (operation-on-abandoned))
      (otherwise
        (destructuring-bind
          (
            &key
            (➜empty     #'use-of-empty)
            &allow-other-keys
            )
          ➜
          (case (status tm)
            ('empty     [➜empty])
            (otherwise  [cant-happen])
            )))))
  
  (defun-typed as ((tm empty-mtm) instance &optional ➜)
    (case (status tm)
      ('active (as (base tm) instance ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise
        (destructuring-bind
          (
            &key
            (➜ok        (be t))
            (➜rightmost (be t))
            (➜no-alloc #'alloc-fail)
            &allow-other-keys
            )
          ➜
          (case (status tm)
            ('empty
              (a&empty tm instance
                {
                  :➜ok (λ()
                         (setf (status tm) 'active) ; this takes the step
                         [➜ok]
                         )
                  :➜no-alloc ➜no-alloc
                  }))            
            ('parked
              (w (base tm) instance)
              (setf (status tm) 'active)
              [➜ok]
              )
            (otherwise  [cant-happen])
            )))))

  ;; we specify these so that we won't lose benefit from the contract
  (defun-typed a&h◨ ((tm empty-mtm) instance &optional ➜)
    (if
      (eq (status tm) 'active)
      (a&h◨ (base tm) instance ➜)
      (a instance ➜)
      ))

  (defun-typed as&h◨ ((tm empty-mtm) instance &optional ➜)
    (if
      (eq (status tm) 'active)
      (as&h◨ (base tm) instance ➜)
      (as instance ➜)
      ))


;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm empty-mtm) instance &optional ➜)
    (case (status tm)
      (('active 'parked) (a◧ (base tm) instance ➜))
      ('empty (a&empty tm instance ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise  [cant-happen])
      ))
        
  (defun-typed d ((tm empty-mtm) &optional spill ➜)
    (case (status tm)
      ('active (d (base tm) spill ➜)) ; due to collisions d can not make the machine empty
      ('parked (d◧ tm instance ➜))
      ('empty
        (destructuring-bind
          (
            &key
            (➜rightmost (be t))
            &allow-other-keys
            )
          ➜
          [➜rightmost]
          ))
      ('abandoned (operation-on-abandoned))
      (otherwise  [cant-happen])
      ))

  ;; status machine adds an additional continuation, that of cont-rigthmost
  ;; this can not happen on machines which always have a head on the tape
  (defun-typed d◧ ((tm empty-mtm) &optional spill ➜)
    (case (status tm)
      ('active (d◧ tm spill ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise
        (destructuring-bind
          (&key
            (➜ok (be t))
            (➜rightmost (be ∅))
            (➜no-alloc #'alloc-fail)
            &allow-other-keys
            )
          ➜
          (case (status tm)
            ('empty [➜rightmost])
            ('parked
              (tape-length-is-one (base tm) ; if so, then we are deleting the last cell
                { :➜t (λ()(d&parked&length-one tm ➜))  ; sets status to empty
                  :➜∅ (λ()
                        (s (base tm) {:ok #'do-nothing :rightmost #'cant-happen})
                        (d◧ (base tm)
                          {:➜ok ➜ok :➜no-alloc ➜no-alloc :➜collision #'cant-happen}
                          ))
                  }))
            (otherwise  [cant-happen])
            )))))

;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
;; interesting nuance here. Normally if two machines are not entangled their heads can not
;; be on the same cell.  However, with status machines if two machines do not share the
;; same tape, but are parked or empty, their heads will compare as being in the same
;; location.  So, say two machines that are not entangled are both parked.  The head is
;; stepped right.  After the step the two machines will compare to be on different cells.
;;
  (defun-typed heads-on-same-cell 
    (
      (tm0 status-tm)
      (tm1 status-tm)
      &optional ➜
      )
      (if
        (∧
          (eq (status tm0) 'active)
          (eq (status tm1) 'active)
          )
        (heads-on-same-cell (base tm0) (base tm1) ➜)
        (if
          (∨
            (eq (status tm0) 'abandoned)
            (eq (status tm1) 'abandoned)
            )
          (operation-on-abandoned)
          (destructuring-bind
            (&key
              (➜t (be t))
              (➜∅ (be ∅))
              &allow-other-keys
              )
            ➜
            (if
              (∨
                (eq (status tm0) 'active)
                (eq (status tm1) 'active)
                )
              [➜∅]
              [➜t] ; both machines are empty or parked at this point
              )))))

  ;; The programmer is not allowed to use the base machine of a status machine
  ;; so a mixed compare between a status machine and a non-status machine is
  ;; peculiar. Surely the programmer kept his contract with us, and such a 
  ;; compare is with an unrelated machine...
  (defun-typed heads-on-same-cell
    (
      (tm0 status-tm)
      (tm1 tape-machine)
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
      (tm0 tape-machine)
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

;;--------------------------------------------------------------------------------
;; nd-tm-generic
;;

  ;; tm0 and tm1 are entangled, thus are of the same tape
  ;; tm0 empty <=> tm1 empty
  ;; tm0 abandoned <=> tm1 abandoned
  ;; tm0 parked, tm1 can be active, vice versa
  (defun-typed s≠ 
    (
      (tm0 empty-mtm)
      (tm1 empty-mtm)
      &optional ➜
      )
    (if
      (∧
        (eq (status tm0) 'active)
        (eq (status tm1) 'active)
        )
      (s≠ (base tm0) (base tm1) ➜)
      (if 
        (eq (status tm0) 'abandoned)
        (operation-on-abandoned)
        (if 
          (eq (status tm0) 'active) ; if tm0 active, then tm1 not active, vice versa
          (s (base tm0) ➜)
          (destructuring-bind
            (&key
              (➜ok (be t))
              (➜rightmost (be ∅))
              (➜bound (be ∅))
              &allow-other-keys
              )
            ➜
            (case (status tm0)
              (('empty 'parked)
                (if
                  (eq (status tm1) 'active)
                  [➜rightmost]
                  [➜bound]
                  ))
              (otherwise  [cant-happen])
              ))))))

  (defun-typed a◨ ((tm empty-mtm) instance &optional ➜)
    (case (status tm)
      (('active 'parked) (a◨ (base tm) instance ➜))
      ('empty (a◧ tm instance ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise  [cant-happen])
      ))
