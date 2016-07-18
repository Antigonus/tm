#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Adds entaglement accounting in to destructive operations and copies.

|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or though a fill machine.
;;

  ;; this is a message sent to all entangled machines to tell them that tm0
  ;; has a new leftmost added to the tape.
  (defun a◧-message (tm-receiver tm0 &optional (cont-ok (be t))))
    (a◧-message-0 tm-receiver (state tm-receiver) tm0 cont-ok)
    )
  (defgeneric a◧-message-0 (tm-receiver state tm0 cont-ok))
  ;; this is typically all that has to be done, but special machines might do something
  ;; different.
  (defmethod a◧-message-0 (tm-receiver (state void) tm0 cont-ok))
    (when (¬ (eq tm-receiver tm0))
      (setf (state tm-receiver) (state tm0)) ; typically will be parked
      (setf (HA tm-receiver) (HA tm0))
      (setf (tape tm-receiver) (tape tm0))
      )
    (funcall cont-ok)
    )
  (defmethod a◧-message-0 (tm-message state tm0 cont-ok)
    (when (¬ (eq tm-message tm0)) (setf (tape tm-message) (tape tm0)))
    (funcall cont-ok)
    )
  ;; for non-singular machine, so there is not state change
  ;; all entangled machines share the same tape
  ;; add a new leftmost for one machine, then update the tape for all others
  (defun ∀-entanglements-a◧-message (tm cont-ok)
    (let(
          (es (entanglements tm))
          )
      (if es
        (progn
          (cue-leftmost es)
          (∀* es (λ(es)(a◧-message (r es) tm)) cont-ok)
          )
        (funcall cont-ok)
        )))

  (defmethod a◧-0 ((tm multi-tape-machine) state object cont-ok cont-no-alloc)
    (call-next-method tm state object
      (λ()(∀-entanglements-a◧-message tm cont-ok))
      cont-no-alloc
      ))

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  ;; as for a◧, we assume that the tape is referenced through its lefmost cell.  When this
  ;; is not true for a given implementation, that implementation will have to further
  ;; specify d◧-1.  So, when we deallocate the leftmost of a tape, we have to update
  ;; the tape reference in all of the entangled machines.  Message, if the leftmost is
  ;; the last cell on the tape, we have to transition to void, and thus up date the state
  ;; recorded in the entangled machines.
  (defun d◧ (
              tm 
              &optional 
              spill 
              (cont-ok #'echo)
              (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
              (cont-collision (λ()(error 'dealloc-entangled)))
              (cont-no-alloc (λ()(error 'alloc-fail)))
              )
    "The leftmost is deallocated independent of where the head is located."
    (d◧-1 tm (state tm) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    )

  ;; this is a message sent to each entangled machine to tell it that d◧ was called on the
  ;; machine given as the tm0 in the args list.  This is used when tm0 remains active after
  ;; removing the leftmost.
  (defun d◧-message (tm-message tm0 &optional (cont-ok (be t)))
    (d◧-message-0 tm-message (state tm-message) tm0 cont-ok)
    )
  (defgeneric d◧-message-0 (tm-message state tm0 cont-ok))
  ;; this is typically all that has to be done, but special machines might do something
  ;; different.
  (defmethod d◧-message-0 (tm-message state  tm0 cont-ok)
    (when (¬ (eq tm-message tm0))
      (setf (tape tm-message) (tape tm0))
      (funcall cont-ok)
      ))
  ;; for non-singular machine, so there is no state change
  ;; all entangled machines share the same tape
  ;; delete leftmost for one machine, then update the tape for all others
  (defun ∀-entanglements-d◧-message (tm cont-ok)
    (let(
          (es (entanglements tm))
          )
      (unless es (return-from ∀-entanglements-d◧-message))
      (cue-leftmost es)
      (∀* es 
        (λ(es)
          (∨
            (eq (r es) tm)
            (d◧-message (r es) tm)
            ))
        cont-ok 
        )))

  (defun d◧-2 (tm dealloc-object cont-ok cont-collision)
    (∃-collision◧ tm
      cont-collision
      (λ() ;; no collision
        (d◧-0 tm
          (λ()
            (∀-entanglements-d◧-message
              tm
              (λ()(funcall cont-ok dealloc-object))
              ))
          ))))

  (defgeneric d◧-1 (tm tm-state spill cont-ok cont-rightmost cont-collision cont-no-alloc))

  (defmethod d◧-1 (tm (tm-state abandoned) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    (declare (ignore tm spill cont-ok cont-rightmost cont-collision cont-no-alloc))
    (error 'operation-on-abandoned)
    )

  (defmethod d◧-1 (tm tm-state spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    (declare (ignore tm-state)) ; r◧ takes care of the void case in this implementation
    (r◧ tm
      (λ(dealloc-object) ; r◧ succeeds, so state will be parked or active
        (singular tm 
          (λ() ; is singular-tape, state transitions to void
            (∀-parked tm
              (λ() ; all parked
                (if
                  spill 
                  (as spill dealloc-object
                    (λ() 
                      (void tm)
                      (funcall cont-ok dealloc-object)
                      )
                    cont-no-alloc
                    )
                  (progn 
                    (void tm)
                    (funcall cont-ok dealloc-object)
                    )))
              cont-collision ; not all parked
              ))
          (λ() ; not singular, so no state change
            (if
              spill 
              ;; if spill 
              (as spill dealloc-object
                (λ() (d◧-2 tm dealloc-object cont-ok cont-collision))
                cont-no-alloc
                )
              ;; if not spill
              (d◧-2 tm dealloc-object cont-ok cont-collision)
              ))))

      cont-rightmost ; r◧ fails, tm must be void
      ))


  (defun d (
             tm 
             &optional 
             spill 
             (cont-ok #'echo)
             (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
             (cont-collision (λ()(error 'dealloc-entangled)))
             (cont-no-alloc (λ()(error 'alloc-fail)))
             )
    "Deallocate the cell just to the right of the head. (A region of length 1.)"
    (d-1 tm (state tm) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    )

  (defgeneric d-1 (tm tm-state spill cont-ok cont-rightmost cont-collision cont-no-alloc))
  (defmethod d-1 (tm (tm-state void) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    (declare (ignore tm tm-state spill cont-ok cont-collision cont-no-alloc))
    (funcall cont-rightmost) ; cont-rightmost follows from a progression of deleting cells from a parked state machine
    )
  (defmethod d-1 (tm (tm-state parked) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    (d◧-1 tm tm-state spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    )
  (defmethod d-1 (tm (tm-state abandoned) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    (declare (ignore tm spill cont-ok cont-rightmost cont-collision cont-no-alloc))
    (error 'operation-on-abandoned)
    )
  (defmethod d-1 (tm (tm-state active) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    (let(
          (tm1 (fork-1 tm))
          )
      (s tm1
        (λ() ; step ok
          (∃-collision tm1
            cont-collision
            (λ() ; no collision
              (let(
                    (dealloc-object (r tm1))
                    )
                (if
                  spill
                  ;; if spill
                  (as spill dealloc-object
                    (λ()
                      (d-0 tm
                        (λ()(funcall cont-ok dealloc-object))
                        ))
                    cont-no-alloc
                    )
                  ;; if not spill
                  (d-0 tm
                    (λ()(funcall cont-ok dealloc-object))
                    ))))))
        cont-rightmost
        )))

