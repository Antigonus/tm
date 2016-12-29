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
;; helpers
;;
  ;; When stepping from a sublist, we step into the sublist to its first instance.  We stack
  ;; the tm of the sublist so that we can return to the sublist and step over it later.
  ;;
  ;; The history buffer should be a stack for depth first.  If it is made a queue, we
  ;; shave off elements at the front of each node.  Going the depth on all first elements,
  ;; walking right, then second elements, etc.
  ;;
  ;; Between steps, destructive oeprations must preserve the nodes that are actively
  ;; descended into, other changes are legal. As examples appending or prepending to
  ;; nodes, or modifying cell values.  If new sublists are added that have not yet
  ;; been stepped to, then they will be stepped to when they are gotten to.
  ;;
  ;; #'deque-sublist is recursive until a tm is found that is not on rightmost and can
  ;; thus be stepped forward (to step over the sublist just traversed).  It would be
  ;; possible to aempty this recursive unwinding by not stacking tm points on rightmost in
  ;; the first place, but this is not as friendly to tree modifications between
  ;; steps. There could be a long time betweeen stacking a tm and getting back to it.
  ;; Perhaps we should provide two versions for s-depth, one constant time but not as
  ;; malable between steps, the other more malable but has that recursive unwind.
  ;;
    (defun s-depth-ru ; ru = recursive unwind
      (
        tm
        history
        &optional
        (cont-s (be 's))
        (cont-si (be 'si))
        (cont-rightmost (be 'rightmost))
        (cont-dequeue (be 'dequeue))
        )
      (labels
        (

          (save-and-step-in() ; a saved traversal point is always one past a sublist
            (enqueue history (fork tm)) ; for later traversal from
            (si tm cont-si #'cant-happen)
            )

          (dequeue-sublist()
            (dequeue history
              (λ(tm0) 
                     (s tm0 ; step past the sublist we just descended into
                  (λ()
                    (cue-to tm tm0)
                    (funcall cont-dequeue)
                    )
                  #'dequeue-sublist ; can't repeat for more than the levels in the tree
                  ))
              cont-rightmost ; there were no saved traversal points to continue from
              ))
          )

        (if (consp (r tm)) ; then it is a sublist
          (save-and-step-in) 
          (s tm cont-s #'dequeue-sublist)
          )))


;;--------------------------------------------------------------------------------
;; properties
;;
;;  we take the deafult and do not support alloc dealloc


;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r-0 ((tm tm-depth) (state active) cont-ok cont-parked) 
    (r (head tm) cont-ok cont-parked)
    )

  (defmethod w-0 ((tm tm-depth) (state active) instance cont-ok cont-parked) 
    (w (head tm) instance cont-ok cont-parked)
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  ;; leftmost for the traversal is defined as the cell that the base
  ;; machine is on when the traversal was initialized
  (defmethod cue-leftmost-0  ((tm tm-depth) (state parked) cont-ok cont-void) 
    (declare (ignore cont-void))
    (void (depth-history (parameters tm)))
    (setf (head tm) (fork (depth-base (parameters tm))))
    )

;;--------------------------------------------------------------------------------
;;  head location predicates
;;
  (defmethod heads-on-same-cell-0
    (
      (tm0 tm-depth) 
      (state0 active)
      (tm1 tm-depth) 
      (state1 active)
      cont-true
      cont-false
      cont-parked
      ) 
    (heads-on-same-cell (head tm0) (head tm1) cont-true cont-false cont-parked)
    )

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defmethod s-0
    (
      (tm tm-depth)
      (state active)
      cont-ok
      cont-rightmost
      )
    (s-depth-ru
      (head tm)
      (depth-history (parameters tm))
      cont-ok
      cont-ok
      cont-rightmost
      cont-ok
      ))

;;--------------------------------------------------------------------------------
;; copying
;;
;; The base copying requies no entanglement accounting, because that is derived.
;; This is for internal use.
;;
  (defmethod cue-to-0
    (
      (tm-cued tm-depth)
      (tm-orig tm-depth)
      )
    (setf (state tm-cued) (state tm-orig))
    (setf (head tm-cued) (fork (head tm-orig)))
    (setf (tape tm-cued) (tape tm-orig))
    (setf (parameters tm-cued) (parameters tm-orig))
    tm-cued
    )

  (defmethod cue-to-0
    (
      (tm-cued tm-depth)
      (tm-orig tape-machine)
      )
    (init tm-cued :base tm-orig)
    tm-cued
    )

  (defmethod cue-to-0
    (
      (tm-cued tape-machine)
      (tm-orig tm-depth)
      )
    (cue-to-0 tm-cued (head tm-orig))
    )

