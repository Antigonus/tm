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
;; tape-machine duplication
;;   we need a layer 0 with no entanglement accounting in order to implement the
;;   entanglement list functions sans circular references.
;;
  ;; cue-to-0 is primitive

  ;; adds entanglement accounting to cue-to-0 result
  (defun cue-to-2
    (
      tm-cued 
      tm-orig
      )
    (cue-to-0 tm-cued tm-orig)
    (let(
          (es (entanglements tm-orig))
          )
      (setf (entanglements tm-cued) es)
      (when es (a (entanglements tm-cued) tm-cued #'do-nothing #'cant-happen))
      )
    tm-cued
    )

  ;; this works when the head is a value, such as an integer or cons.  However, if it is a
  ;; reference, then a deeper copy will be needed. Note for example, tm-region
  (defun cue-to (tm-cued tm-orig)
    "tm-cued machine will be rewritten.  It will be change-class'ed to the same type as
     tm-orig, it will share the same tape, entanglesments, and parameters as tm-orig,
     though have an indendent head.  The head is initially on the same cell as that of
     tm-orig.  tm-cued is added to the entanglement list.
     "
    (disentangle tm-cued) ; the entangled machines will no longer see tm-cued
    (change-class tm-cued (type-of tm-orig))
    (cue-to-2 tm-cued tm-orig)
    tm-cued
    )

  (defun dup (tm-orig)
    "Returns a new tm cued to tm-orig."
    (let(
          (tm-cued (make-instance (type-of tm-orig)))
          )
      (cue-to-2 tm-cued tm-orig)
      tm-cued
      ))

  ;; Mounts the same tape that another machine has mounted.
  ;; Unlike dup, upon exit the head is at leftmost.
  (defmethod mount ((tm tape-machine) &optional (cont-ok #'echo) cont-fail)
    (declare (ignore cont-fail))
    (let(
          (tm-dup (dup tm))
          )
      (cue-leftmost tm-dup)
      (funcall cont-ok tm-dup)
      ))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
;; Allocated cells must be initialized.  The initialization value is provided
;; directly or though a fill machine.
;;
  ;; file local utility function
  (defun update-tape (entanglements new-tape)
    (cue-leftmost entanglements)
    (⟳(λ(cont-loop cont-return)
        (let(
              (entangled-machine (r entanglements))
              )
          (setf (tape entangled-machine) new-tape)
          (s entanglements cont-loop cont-return)
          ))))

  (defgeneric a◨ (tm object &optional cont-ok cont-no-alloc)
    (:documentation
      "Allocates a cell to the right of rightmost (thus making a new rightmost)."
      ))

  ;; entangled machines must literally share the same tape
  (defmethod a◨
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (a◧-0 tm (state tm) object
      (λ()
        (let(
              (entanglements (entanglements tm))
              )
          (when entanglements
            (update-tape entanglements (tape tm)); redundant update for tm doesn't hurt
            ))
        (funcall cont-ok)
        )
      cont-no-alloc
      ))

  (defgeneric a◨s (tm object &optional cont-ok cont-no-alloc)
    (:documentation
      "Allocates a cell to the right of rightmost, and steps to it"
      ))

  (defmethod a◨s
    (
      (tm tape-machine)
      object
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (a◨ tm object
      (λ() (s tm cont-ok #'cant-happen))
      cont-no-alloc
      ))

  (defgeneric -a (tm object &optional cont-ok cont-no-alloc)
    (:documentation 
      "Allocate a new cell to the left of the head."
      ))

  (defmethod -a (tm object &optional cont-ok cont-no-alloc)
    (a tm (r tm) 
      (λ()
        (w tm object)
        (s tm
          cont-ok
          (λ()(error 'tm-impossible-to-get-here :text "we just called #'a"))
          ))
      cont-no-alloc
      ))

  (defgeneric -a-s (tm object &optional cont-ok cont-no-alloc)
    (:documentation 
      "Allocate a new cell to the left of the head.  Then step left."
      ))

  (defmethod -a-s (tm object &optional cont-ok cont-no-alloc)
    (a tm (r tm) 
      (λ() 
        (w tm object)
        (funcall cont-ok)
        )
      cont-no-alloc
      ))

;;--------------------------------------------------------------------------------
;; cell deallocation
;;

  (defun d◧-∀-void (entanglements)
    (when entanglements
      (cue-leftmost entanglements)
      (⟳(λ(cont-loop cont-return)
          (let(
                (entangled-machine (r entanglements))
                )
            (change-class entangled-machine 'tm-void)
            ;; head continues to hold the type
            (setf (tape entangled-machine) ∅) ; frees the tape memory
            ;; entanglements are preserved
            )
          (s entanglements cont-loop cont-return)
          ))))

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
      "Similar to #'d but the leftmost cell is deallocated independent of where the head
       is located. If the tape is singleton, calling d◧ will cause the machine to collapse
       to void.
       "
    (r◧ tm
      (λ(dealloc-object)
        (∃-collision◧ tm
          cont-collision
          (λ() 
            (let(
                  (entanglements (entanglements tm))
                  )
              (singleton tm 
                (λ() ; is singleton-tape, goes to void
                  (if
                    spill 
                    (as spill dealloc-object
                      (λ() 
                        (d◧-∀-void entanglements)
                        (funcall cont-ok)
                        )
                      cont-no-alloc
                      )
                    (progn 
                      (d◧-∀-void entanglements)
                      (funcall cont-ok)
                      )))
                (λ() ; not singleton
                  (d◧-0 tm (state tm)
                    (λ()
                      (if
                        spill 
                        (as spill dealloc-object
                          (λ() 
                            (update-tape entanglements (tape tm))
                            (funcall cont-ok)
                            )
                          cont-no-alloc
                          )
                        (progn
                          (update-tape entanglements (tape tm))
                          (funcall cont-ok)
                          )))
                    cont-not-supported
                    ))
                )))))
      cont-rightmost ; reading void, limiting case of deleting cells from a tape
      ))

;;--------------------------------------------------------------------------------
;; moving data
;;  --move this to src-2

  ;; In repeated move operations we probably throw the displaced objects away if the
  ;; programmer wants to keep them xhe should copy them first, complications with
  ;; implementing this more efficiently on lists due to head cell locations with shared
  ;; tapes. In any case with repeated ops we can hop n places instead of shuffling.
  ;;
    (defgeneric m (tm fill)
      (:documentation
        "The object in rightmost is returned.
         All other objects on the tape move right one cell.
         Leftmost is written with the provided fill-object. 
         "
        ))

    (defmethod m 
      (
        (tm tape-machine)
        fill-object
        )
      (⟳ (λ(cont-loop cont-return)
           (let((displaced-object (r tm)))
             (w tm fill-object)
             (setf fill-object displaced-object)
             (s tm cont-loop cont-return)
             )))
      fill-object
      )

