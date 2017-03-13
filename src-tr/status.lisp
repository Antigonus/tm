#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Wraps another tape machine so as to provide and use status information 
about the lower machine.  Status is either 'empty, 'parked, 'active.

The basic machines 'src-list' and others, require having a value to 
build the machine from. With this wrapper we can build an empty machine.

We own the base machine, and have a contract with the programmer that
he/she will not use it.  Hence when we set the head of the base machine
at the leftmost cell, it stays there.

The right neighbor of a parked head, is the leftmost cell of the base machine's tape.

If we include 'abandoned as a status, then the entire interface will have to be reproduced
here, and inheriting from identity does very little.  But the same can be said of
'empty. So we may as well keep abandoned.  .. well at least we get to inherit base slot
from identity..

In this implementation, the base for an empty machine has one cell.  For an empty machine
I put the value nil into that cell, and the base machine has to allow us to that.  In this
manner I have implemented an actual wrapper.  There are no modifications to the base
machine interface.

I also considered another implementation where I had a make-empty and add-first function
for every machine.  So for example make-empty for a list machine made the head and tape
nil, and the add first then recreated the structure. After making a machine empty list-tm
code would break if run with such an instance, but the status machine protected the base
instance from being used as a paramter -- almost.  We still had entanglement copy.
Entanglement copy does not touch the head or the tape, so actually that would have been
safe.  Indeed we have made it illegal for the outside to use the instance.  The only
drawbacks were in maintaining make-empty and add-first for every implementation, and
maintaining the extra complexity.  Perhaps in a later version we will do this.


|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a tape machine
;;
  (def-type status-tr (identity-tr)
    (
      (status
        :initarg status
        :accessor status
        )
      ))

  (defun is-legal-status (status &optional (ct (be t)) (c∅ (be ∅)))
    (case status
      (('abandoned 'empty 'parked 'active) [ct])
      (otherwise [c∅])
      ))

;;--------------------------------------------------------------------------------
;; making status transforms
;;
  (defun-typed init 
    (
      (tm0 status-tr)
      (init-value cons)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (destructuring-bind
        (&key base type tape) init-value
        (cond
          (base
            (setf (base tm0) base)
            (setf (status tm0) 'active)
            [➜ok tm0]
            )
          ((∧ base type) [➜fail])
          ((∧ type tape) 
            (mk type {:tape tape}
              {
                :➜ok (λ(tm1)
                       (setf (base tm0) tm1)
                       (setf (status tm0) 'active)
                       [➜ok tm0]
                       )
                :➜fail [➜fail]
                :➜no-alloc [➜no-alloc]
                }))
          (type ;make an empty machine
            (mk type {:tape {∅}}
              {
                :➜ok (λ(tm1)
                       (setf (base tm0) tm1)
                       (setf (status tm0) 'empty)
                       [➜ok tm0]
                       )
                :➜fail [➜fail]
                :➜no-alloc [➜no-alloc]
                }))
          (t [➜fail])
          ))))

  (defun-typed init ; intializes tm0 to have an entangled copy of tm1's base
    (
      (tm0 status-tr)
      (tm1 status-tr) ; init-value
      &optional ➜
      )
    (destructing-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (if
        (eq (status init-value) 'abandoned)
        (operation-on-abandoned)
        (mk (type-of (base tm1)) (base tm1)
          {:➜ok (λ(tm1-base-copy)
                  (setf (base tm0) tm1-base-copy)
                  (setf (status tm0) (status tm1))
                  [➜ok tm]
                  )
            :➜fail [:➜fail]
            :➜no-alloc [:➜no-alloc]
            }))))

;;--------------------------------------------------------------------------------
;; abandon machine
;;
  (def-function-class abandon (tm))

  (defun-typed abandon ((tm status-tm))
    (setf (status tm) 'abandoned)
    (abandon (base tm))
    )

  (defun-typed abandon ((tm list-tm))
    (setf (head tm) ∅) ; we release these for garbage collection
    (setf (tape tm) ∅)
    )

;;--------------------------------------------------------------------------------
;; park head
;;
  (def-function-class park (tm))

  (defun-typed park ((tm status-tm))
    (setf (status tm) 'park)
    (c◧ (base tm)) ; base machine head must always be on a cell
    )

;;--------------------------------------------------------------------------------
;; make the machine empty
;;   an empty machine has all of its container structure.  As the expense of building that
;;   structure is incurred when the empty machine is created, not upon the first write.
;;   That way the programmer can control when that expense occurs.
;;
;;   Perhaps we should not write a nil value to the last cell when becoming empty, because
;;   we don't know if it is legal to do so on the given machine.  Suppose for example,
;;   some machine was counting writes, or searching for nil writes, for a special reason,
;;   then we would trip the filter. But we might want to have this last value garbage
;;   collected, and this seems to be more likely to be important.
;;

  ;; the machine is parked, tape is length-1, now we delete the last cell
  (def-function-class d&parked&length-one (tm &optional ➜))

  (defun-typed d&parked&length-one ((tm status-tm) &optional ➜)
    (setf (status tm) 'empty)
    (w (base tm) ∅ ➜) ; so that the instance held there can be garbage collected
    )

;;--------------------------------------------------------------------------------
;; add the first cell to an empty machine
;;
  (def-function-class a&empty (tm instance) &optional ➜)

  (defun-typed a&empty ((tm status-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
        ➜
      (w (base tm) instance
        {:➜ok (λ()
                (setf (status tm) 'parked)
                [➜ok]
                )
          :➜no-alloc #'➜no-alloc
          })))


;;--------------------------------------------------------------------------------
;; scoped make
;;
  (defun with-mk-status
    (
      args
      continuation
      )
    (let(
          (tm (mk 'tm-status args))
          )
      (unwind-protect
        [continuation tm]
        (self-
      

    )

    
;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defmacro def-status-tr-1 (f &rest args)
    `(case (status tm)
       ('active (,f (base tm) ,@args ➜))
       ('abandoned (operation-on-abandoned))
       (otherwise
         (destructuring-bind
           (
             &key
             (➜empty     #'use-of-empty)
             (➜parked    #'parked-head-use)
             &allow-other-keys
             )
           ➜
           (case (status tm)
             ('empty     [➜empty])
             ('parked    [➜parked])
             (otherwise  [cant-happen])
             )))))

  (def-status-tr-1 r)

  (defun-typed esr ((tm status-tr) &optional ➜)
    (case (status tm)
      ('active (esr (base tm) ➜))
      ('parked (r (base tm) ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise
        (destructuring-bind
          (
            &key
            (➜rightmost (be ∅))
            &allow-other-keys
            )
          ➜
          (case (status tm)
            ('empty     [➜rightmost])
            (otherwise  [cant-happen])
            )))))

  (def-status-tr-1 w instance)

  (defun-typed esw ((tm status-tr) instance &optional ➜)
    (case (status tm)
      ('active (esw (base tm) ➜))
      ('parked    (w (base tm) ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise
        (destructuring-bind
          (
            &key
            (➜rightmost (be ∅))
            &allow-other-keys
            )
          ➜
          (case (status tm)
            ('empty     [➜rightmost])
            (otherwise  [cant-happen])
            )))))

  (defun-typed c◧ ((tm status-tr) &optional ➜)
    (case (status tm)
      ('active (c◧ (base tm) ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise
        (destructuring-bind
          (
            &key
            (➜ok (be t))
            (➜empty #'use-of-empty)
            &allow-other-keys
            )
          ➜
          (case (status tm)
            ('empty     [➜empty])
            ('parked
              ;; base machine will already have its head on leftmost, all parked machines do
              (setf (status tm) 'active)
              [➜ok]
              )
            (otherwise  [cant-happen])
            )))))

  (defun-typed s ((tm status-tr) &optional ➜)
    (case (status tm)
      ('active (s (base tm) ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise
        (destructuring-bind
          (
            &key
            (➜ok        (be t))
            (➜rightmost (be ∅))
            &allow-other-keys
            )
          ➜
          (case (status tm)
            ('empty     [➜rightmost])
            ('parked
              (setf (status tm) 'active)
              [➜ok]
              )
            (otherwise  [cant-happen])
            )))))

  (defun-typed a ((tm status-tr) instance &optional ➜)
    (case (status tm)
      ('active (a (base tm) instance ➜))
      ('parked (a◧ (base tm) instance ➜))
      ('empty  (a&empty tm ➜)) ; will change status to parked
      ('abandoned (operation-on-abandoned))
      (otherwise  [cant-happen])
      ))

  (defun-typed on-leftmost ((tm status-tr) &optional ➜)
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

  (defun-typed on-rightmost ((tm status-tr) &optional ➜)
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
  (defun-typed c◨ ((tm status-tr) &optional ➜)
    (case (status tm)
      ('active (c◨ (base tm) ➜))
      ('parked
        (setf (status tm) 'active)
        (c◨ (base tm) ➜)
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
  
  (defun-typed as ((tm status-tr) instance &optional ➜)
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
  (defun-typed a&h◨ ((tm status-tr) instance &optional ➜)
    (if
      (eq (status tm) 'active)
      (a&h◨ (base tm) instance ➜)
      (a instance ➜)
      ))

  (defun-typed as&h◨ ((tm status-tr) instance &optional ➜)
    (if
      (eq (status tm) 'active)
      (as&h◨ (base tm) instance ➜)
      (as instance ➜)
      ))


;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm status-tr) instance &optional ➜)
    (case (status tm)
      (('active 'parked) (a◧ (base tm) instance ➜))
      ('empty (a&empty tm instance ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise  [cant-happen])
      ))
        
  (defun-typed d ((tm status-tr) &optional spill ➜)
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
  (defun-typed d◧ ((tm status-tr) &optional spill ➜)
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
      (tm0 status-tr)
      (tm1 status-tr)
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

  (defun-typed a◨ ((tm status-tr) instance &optional ➜)
    (case (status tm)
      (('active 'parked) (a◨ (base tm) instance ➜))
      ('empty (a◧ tm instance ➜))
      ('abandoned (operation-on-abandoned))
      (otherwise  [cant-happen])
      ))
