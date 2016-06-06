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
          (tm1 (fork-0 tm))
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
  ;; leftmost cell, so all references to the tape by entangled machines need to be updated
  ;; after a new leftmost cell is added. If the tape is referenced in another manner in
  ;; some other tape machine implementation, then a◧-1 and/or a◧-also-0 must be further
  ;; specified for that other implementation.
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

  ;; this is a message sent to all entangled machines to tell them that tm0
  ;; has been had a new leftmost cell added to the tape.
  (defun a◧-also (tm-also tm0 &optional (cont-ok (be t)) (cont-not-supported (be ∅)))
    (a◧-also-0 tm-also (state tm-also) tm0 cont-ok cont-not-supported)
    )
  (defgeneric a◧-also-0 (tm-also state tm0 cont-ok cont-not-supported))
  ;; this is typically all that has to be done, but special machines might do something
  ;; different.
  (defmethod a◧-also-0 (tm-also (state void) tm0 cont-ok cont-not-supported)
    (declare (ignore cont-not-supported))
    (when (¬ (eq tm-also tm0))
      (setf (state tm-also) (state tm0)) ; typically will be parked
      (setf (HA tm-also) (HA tm0))
      (setf (tape tm-also) (tape tm0))
      (funcall cont-ok)
      ))
  (defmethod a◧-also-0 (tm-also state tm0 cont-ok cont-not-supported)
    (declare (ignore cont-not-supported))
    (when (¬ (eq tm-also tm0))
      (setf (tape tm-also) (tape tm0))
      (funcall cont-ok)
      ))
  ;; for non-singleton machine, so there is not state change
  ;; all entangled machines share the same tape
  ;; delete leftmost for one machine, then update the tape for all others
  (defun ∀-entanglements-a◧-also (tm cont-ok cont-not-supported)
    (let(
          (es (entanglements tm))
          )
      (unless es (return-from ∀-entanglements-a◧-also))
      (cue-leftmost es)
      (∀* es (λ(es)(a◧-also (r es) tm)) cont-ok cont-not-supported)
      ))

  (defgeneric a◧-1 (tm state object cont-ok cont-not-supported cont-no-alloc))
  (defmethod a◧-1 (tm (state void) object cont-ok cont-not-supported cont-no-alloc)
    (a◨-0 tm state object
      (λ()(∀-entanglements-a◧-also tm cont-ok cont-not-supported))
      cont-not-supported
      cont-no-alloc
      ))
  (defmethod a◧-1 (tm state object cont-ok cont-not-supported cont-no-alloc)
    (a◨-0 tm state object
      (λ()
        (λ()(∀-entanglements-a◧-also tm cont-ok cont-not-supported))
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

  ;; this is a message sent to each entangled machine to tell it that d◧ was called on the
  ;; machine given as the tm0 in the args list.  This is used when tm0 remains active after
  ;; removing the leftmost cell.
  (defun d◧-also (tm-also tm0 &optional (cont-ok (be t)) (cont-not-supported (be ∅)))
    (d◧-also-0 tm-also (state tm-also) tm0 cont-ok cont-not-supported)
    )
  (defgeneric d◧-also-0 (tm-also state tm0 cont-ok cont-not-supported))
  ;; this is typically all that has to be done, but special machines might do something
  ;; different.
  (defmethod d◧-also-0 (tm-also state  tm0 cont-ok cont-not-supported)
    (declare (ignore cont-not-supported))
    (when (¬ (eq tm-also tm0))
      (setf (tape tm-also) (tape tm0))
      (funcall cont-ok)
      ))
  ;; for non-singleton machine, so there is not state change
  ;; all entangled machines share the same tape
  ;; delete leftmost for one machine, then update the tape for all others
  (defun ∀-entanglements-d◧-also (tm cont-ok cont-not-supported)
    (let(
          (es (entanglements tm))
          )
      (unless es (return-from ∀-entanglements-d◧-also))
      (cue-leftmost es)
      (∀* es (λ(es)(d◧-also (r es) tm)) cont-ok cont-not-supported)
      ))

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
                      (void tm)
                      (funcall cont-ok dealloc-object)
                      )
                    cont-no-alloc
                    )
                  (progn 
                    (void tm)
                    (funcall cont-ok dealloc-object)
                    )))
              (λ() ; not singleton, so no state change
                (if
                  spill 
                  ;; if spill 
                  (as spill dealloc-object
                    (λ() 
                      (d◧-0 tm (state tm)
                        (λ()
                          (∀-entanglements-d◧-also
                            tm
                            (λ()(funcall cont-ok dealloc-object))
                            cont-not-supported ; an entangled machine claims d◧ is not supported
                            ))
                        cont-not-supported
                        ))
                    cont-no-alloc
                    )
                  ;; if not spill
                  (d◧-0 tm (state tm)
                    (λ()
                      (∀-entanglements-d◧-also
                        tm
                        (λ()(funcall cont-ok dealloc-object))
                        cont-not-supported ; an entangled machine claims d◧ is not supported
                        ))
                    cont-not-supported
                    )))))))
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
                      (d-0 tm state
                        (λ()(funcall cont-ok dealloc-object))
                        cont-not-supported
                        ))
                    cont-no-alloc
                    )
                  ;; if not spill
                  (d-0 tm state
                    (λ()(funcall cont-ok dealloc-object))
                    cont-not-supported
                    ))))))
        cont-rightmost
        )))

