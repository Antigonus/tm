#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions derive the remainder of the tape-machine interface while using only the
primitives from tm-primitives.  

There is no functional need for a new tape machine implementation to specialize these
functions.  Still, some implementations will want to specialize these functions for
performance reasons.

Because these are built upon the primitives, they can only be tested against implementations
of the primitives.


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or though a fill machine.
;;
  (defun a◨
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Allocates a cell to the right of rightmost (thus becoming the new rightmost)."
    (a◨-0 tm (state tm) object cont-ok cont-not-supported cont-no-alloc)
    )
  (defgeneric a◨-0 (tm state object cont-ok cont-not-supported cont-no-alloc))
  (defmethod a◨-0 (tm (state void) object cont-ok cont-not-supported cont-no-alloc)
    (declare (ignore state))
    ;; ironic, allocating to rightmost from void is the same as allocating to leftmost
    (a◧ tm object cont-ok cont-no-alloc)
    )
  (defmethod a◨-0 (tm state object cont-ok cont-not-supported cont-no-alloc)
    (declare (ignore state))
    (let(
          (tm1 (copy-0 tm))
          )
      (cue-rightmost tm1)
      (a tm1 object cont-ok cont-no-alloc)
      ))

  (defun a◨s
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Allocates a cell to the right of rightmost, and steps to it"
    (a◨ tm object
      (λ() (s tm cont-ok #'cant-happen))
      cont-not-supported
      cont-no-alloc
      ))

  ;; all entangled machines share the same tape, but typically reference the tape by its
  ;; leftmost cell, so the references to the tape need to be updated after a new leftmost
  ;; cell is added. If the tape is referenced in another manner in some other tape machine
  ;; implementation, then a◧-1 must be further specified for that other implementation.
  ;; If tm is void, then adding a cell to the tape will transition the machine to parked
  (defun a◧
    (
      tm
      object
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    "Allocates a cell to the left of leftmost (thus becoming the new leftmost)."
    (a◧-1 tm (state tm) object cont-ok cont-not-supported cont-no-alloc)
    )

  (defun entanglements-follow-0 (tm)
        (let(
              (tape (tape tm))
              (entanglements (entanglements tm))
              )
          (∀ entanglements 
            (λ(es)
              (setf (tape (r es)) tape)
              t
              ))
          ))

  (defun entanglements-follow-1 (tm)
        (let(
              (tape (tape tm))
              (state (state tm))
              (entanglements (entanglements tm))
              )
          (∀ entanglements 
            (λ(es)
              (setf (tape (r es)) tape)
              (setf (state (r es)) state)
              t
              ))
          ))

  (defgeneric a◧-1 (tm state object cont-ok cont-not-supported cont-no-alloc))
  (defmethod a◧-1 (tm (state void) object cont-ok cont-not-supported cont-no-alloc)
    (a◨-0 tm state object
      (λ()
        (entanglements-follow-1 tm)
        (funcall cont-ok)
        )
      cont-not-supported
      cont-no-alloc
      ))
  (defmethod a◧-1 (tm state object cont-ok cont-not-supported cont-no-alloc)
    (a◨-0 tm state object
      (λ()
        (entanglements-follow-0 tm)
        (funcall cont-ok)
        )
      cont-not-supported
      cont-no-alloc
      ))

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  ;; as for a◧, we assume that the tape is referenced through its lefmost cell.  When this
  ;; is not true for a given implementation, that implementation will have to further
  ;; specify d◧-1.  So, when we deallocate the leftmost cell of a tape, we have to update
  ;; the tape reference in all of the entangled machines.  Also, if the leftmost cell is
  ;; the last cell on the tape, we have to transition to void, and thus up date the state
  ;; recorded in the entangled machines.
  (defun d◧ (
              tm 
              &optional 
              spill 
              (cont-ok #'echo)
              (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
              (cont-not-supported (λ()(error 'not-supported)))
              (cont-collision (λ()(error 'dealloc-entangled)))
              (cont-no-alloc (λ()(error 'alloc-fail)))
              )
    "The leftmost cell is deallocated independent of where the head is located."
    (d◧-1 tm (state tm) spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc)
    )

  ;; I would have made this generic, but chances are if one needs to specify a 
  ;; different version, one will be respecifying d◧-1 also (where this is called from).
  (defun to-void (tm)
    (setf (state tm) void)
    (setf (HA tm) ∅)
    (setf (tape tm) ∅)
    (entanglements-follow-1 tm)
    )

  (defgeneric d◧-1 (tm state spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc))
  (defmethod d◧-1 (tm state spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc)
    (r◧ tm
      (λ(dealloc-object)
        (∃-collision◧ tm
          cont-collision
          (λ() 
            (singleton tm 
              (λ() ; is singleton-tape, state transitions to void
                (if
                  spill 
                  (as spill dealloc-object
                    (λ() 
                      (to-void tm)
                      (funcall cont-ok)
                      )
                    cont-no-alloc
                    )
                  (progn 
                    (to-void tm)
                    (funcall cont-ok)
                    )))
              (λ() ; not singleton, so no state change
                (d◧-0 tm (state tm)
                  (λ()
                    (if
                      spill 
                      (as spill dealloc-object
                        (λ() 
                          (entanglements-follow-0 tm)
                          (funcall cont-ok)
                          )
                        cont-no-alloc
                        )
                      (progn
                        (entanglements-follow-0 tm)
                        (funcall cont-ok)
                        )))
                  cont-not-supported
                  )))
            )))
      cont-rightmost ; reading void, limiting case of deleting cells from a tape
      ))

  (defun d (
             tm 
             &optional 
             spill 
             (cont-ok #'echo)
             (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
             (cont-not-supported (λ()(error 'not-supported)))
             (cont-collision (λ()(error 'dealloc-entangled)))
             (cont-no-alloc (λ()(error 'alloc-fail)))
             )
    "Deallocate the cell just to the right of the head. (A region of length 1.)"
    (d-1 tm (state tm) spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc)
    )

  (defgeneric d-1 (tm state spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc))
  (defmethod d-1 (tm (state void) spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc)
    (declare (ignore tm state spill cont-ok cont-not-supported cont-collision cont-no-alloc))
    (funcall cont-rightmost) ; cont-rightmost follows from a progression of deleting cells from a parked state machine
    )
  (defmethod d-1 (tm (state parked) spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc)
    (d◧-1 tm state spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc)
    )
  (defmethod d-1 (tm (state active) spill cont-ok cont-rightmost cont-not-supported cont-collision cont-no-alloc)
    (csnr tm 1
      (λ(dealloc-object) 
        (let(
              (tm1 (copy-1 tm))
              )
          (∃-collision tm1
            cont-collision
            (λ() 
              (d-0 tm state 
                (λ() ; cont-ok for d-0
                  (if
                    spill 
                    (as spill dealloc-object
                      (λ() 
                        (entanglements-follow-0 tm)
                        (funcall cont-ok)
                        )
                      cont-no-alloc
                      )
                    (progn
                      (entanglements-follow-0 tm)
                      (funcall cont-ok)
                      )))
                cont-not-supported
                )))))
      cont-rightmost
      #'cant-happen
      ))

