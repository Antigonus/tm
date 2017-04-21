#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  A second order structure to add status to a machine.

  First order machines only exist when there is something to put in them.  Here I
  introduce a second order concept of emptiness.  A first order machine becomes empty when
  it is first parked, and then the last cell is requested to be deleted.  When this
  happens, we change-class the second order machine to be the status-empty machine, and
  the machine will be functionally empty from the point of view of the interface.
  However, the base first order machine will still be there, and will still have one cell.
  For sake of memory efficiency, we invoke garbage collection for the instance held in
  this one cell by writing nil to it.  Consequently, as an artificial constraint, all
  first order machines must be capable of having a single cell that allows one to write
  nil to it.  This might be a challenge for some abstract machines.

  I had also considered, and have in the past implemented, nulling out the base slot when
  the base machine becomes empty, but then when a new cell is added we must recreate the
  base machine, which incurs overhead and requires knowing its type and initialization
  parameters.  Knowing the base machine's type is rather simple, as we can store it in a
  slot, but the recreation overhead is unknown, and it might not be possible to know how to
  correctly initialize the recreated machine. I suppose we could add a 'this is how to
  recreate me' command.  Anyway, the current implementation voids all that complexity
  by just keeping the base machine around with a single cell (that as the last action we
  wrote nil to). 

  We adopt a convention that, for a parked status-tm, the head of the base machine will be
  on leftmost.

Currently these are supported status:

abandoned
parked
empty
active

There is no function on the tm interface that can be called to change the status
of an active machine.  'delete' of the last cell, for example, will result in 
a collision error.  Hence behavior is inherited from the identity transform.

---

  No instance is ever made of the type status-parked-active.  Rather functions are
  declared to match tm's of this type.  We include status-parked-active in the
  inheritance tree, so that the dispatch will check these functions from the parked or
  from active state before generalizing further.


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;
  (def-type status-tm (identity-tr)
    (
      (base ; the machine being managed
        :initarg :base
        :accessor base
        )
      (address ; an integer address locating the head
        :initarg :address
        :accessor address
        )
      (address-rightmost ; address of the rightmost cell
        :initarg :address-rightmost
        :accessor address-rightmost
        )
      ))

  (def-type status-abandoned (status-tm)())
  (def-type status-active    (status-tm)())
  (def-type status-empty     (status-tm)())
  (def-type status-parked    (status-tm)())


;;--------------------------------------------------------------------------------
;; state transition functions
;;
;;   these are private functions used by status code and specializations
;;   we need these so that specialized types can change to their specialized versions for
;;   example an ea machine will have a to-empty that goes to 'ea-empty instead of to
;;   status-empty
;;
  (def-function-class to-abandoned (tm))
  (def-function-class to-active    (tm))
  (def-function-class to-empty     (tm))
  (def-function-class to-parked    (tm))

  (defun-typed to-abandoned ((tm status-tm)) (change-class tm 'status-abandoned))
  (defun-typed to-active    ((tm status-tm)) (change-class tm 'status-active))
  (defun-typed to-empty     ((tm status-tm)) (change-class tm 'status-empty))
  (defun-typed to-parked    ((tm status-tm)) (change-class tm 'status-parked))

;;--------------------------------------------------------------------------------
;;
  (defun-typed init
    (
      (tm status-tm)
      &optional
      keyed-parms
      ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        &allow-other-keys
        )
      ➜
      (destructuring-bind
        (&key base status &allow-other-keys) keyed-parms
        (cond

          ;; base machine is to be marked empty. Would like to delete cells to force
          ;; empty, but base may be nd type, so instead we enforce the rule that base
          ;; machines that are declared ':empty' must have only one cell (deletion must be
          ;; external)
          ((∧ 
             base
             (typep base 'tape-machine) 
             (eq status 'status-empty) 
             (tape-length-is-one base)
             )
            (w base ∅)
            (setf (base tm) base) ; should call next method and have idenity do this ..
            (setf (address-rightmost tm) 0)
            (setf (address tm) 0)
            (to-empty tm)
            [➜ok tm]
            )

          ;; base machine is not empty
          ;; We don't know if the base machine can be entangled, so to get the
          ;; address counters we cue the machine to leftmost, and thus lose the
          ;; original head location
          ((∧ 
             base
             (typep base 'tape-machine)
             (∨ (¬ status) (eq status 'status-active) (eq status 'status-parked))
             )
            (let(
                  (cell-address 0)
                  )
              (c◧ base)
              (s base {:➜ok (λ()
                              (∀* base (λ(base)(declare (ignore base))(incf cell-address)))
                              )})
              (setf (address-rightmost tm) cell-address)
              (c◧ base)
              (setf (address tm) 0)
              (setf (base tm) base)
              (if
                (∨ (¬ status) (eq status 'status-active))
                (to-active tm)
                (to-parked tm)
                )
              [➜ok tm]
              ))

          (t [➜fail])
          ))))

  (defun-typed entangle ((tm-orig status-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        ;; (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜  
      ;; (prins (print "entangle status-tm"))
      (call-next-method tm-orig
        {
          :➜ok (λ(tm-entangled)
                 (setf (address tm-entangled) (address tm-orig))
                 (setf (address-rightmost tm-entangled) (address-rightmost tm-orig))
                 [➜ok tm-entangled]
                 )
          (o (remove-key-pair ➜ :➜ok))
          })))

  (def-function-class park (tm &optional ➜)) ; handled by subtypes

  (def-function-class abandon (tm))

  (defun-typed abandon ((tm status-tm))
    (setf (base tm) ∅)
    (to-abandoned tm)
    )

  (defun-typed with-entangled ((tm status-tm) continuation)
    (let(
          (etm (entangle tm))
          )
      (unwind-protect
        [continuation etm]
        (abandon etm)
        )))
