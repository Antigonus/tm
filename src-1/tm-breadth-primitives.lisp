#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The machine's tape has been weaved through a tree.

  This machine must be initialized to another tape before being used.  It
  can not be parked.

  We include the base tm as a slot, rather than inheriting it, because we need the
  specialized behavior of the tm methods inside the implementation of tm-depth methods.
|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-breadth (tape-machine)())

;;--------------------------------------------------------------------------------
;; helpers
;;
  ;; When stepping on an cell with an object that is a sublist we queue it so that we can
  ;; come back to it later.
  ;;
  ;; The history buffer should be a queue.  If it is made a stack we get a depth
  ;; first search along the right side of the tree, instead of along the left.
  ;;
  ;; Between steps, atoms in the tree may be deleted, and sublists or atoms added, as
  ;; long as it is possible to still take a correct next step (whatever that step is
  ;; now supposed to be) from the cell under the head.  For list structures this means
  ;; that (cdr (HA tm)) must be valid.
  ;;
    (defun s-breadth 
      (
        tm
        history
        &optional
        (cont-s (be 's))
        (cont-si (be 'si)) ; implies a deque operation
        (cont-rightmost (be 'rightmost))
        )
      (labels
        (

          (save-sublist ()
            (enqueue history (dup tm))
            )
          
          (dequeue-and-step-in ()
            (dequeue history
              (λ(tm0)
                (cue-to tm tm0)
                (si tm cont-si #'cant-happen) ; we only save lists
                )
              cont-rightmost ; there were no sublists to explore
              ))
          
          )
        (when (consp (r tm)) (save-sublist)) ; queue for later stepping into
        (s
          tm
          cont-s
          #'dequeue-and-step-in
          )
        ))

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r ((tm tm-breadth)) (r (tape tm)))
  (defmethod w ((tm tm-breadth) object) (w (tape tm) object))

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  ;; our tape is never nil, so this returns true
  (defmethod cue-leftmost  ((tm tm-breadth)) 
    (cue-leftmost (tape tm))
    )

;;--------------------------------------------------------------------------------
;;  head location predicates
;;
  (defmethod heads-on-same-cell 
    (
      (tm0 tm-breadth) 
      (tm1 tm-breadth) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (heads-on-same-cell (tape tm0) (tape tm1) cont-true cont-false)
    )

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defmethod s
    (
      (tm tm-breadth)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (s-breadth
      (tape tm)
      (HA tm)
      cont-ok
      cont-ok
      cont-rightmost
      ))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defmethod a 
    (
      (tm tm-breadth)
      object 
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (a (tape tm) object cont-ok cont-no-alloc)
    )

;;--------------------------------------------------------------------------------
;; deallocating cells
;;
  ;; deallocates the cell just to the right of the head
  (defmethod d 
    (
      (tm tm-breadth)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
      (cont-not-supported (λ()(error 'dealloc-not-supported)))
      (cont-entangled (λ()(error 'dealloc-entangled)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (d (tape tm) spill
      cont-ok 
      cont-rightmost
      cont-not-supported
      cont-entangled
      cont-no-alloc
      ))
