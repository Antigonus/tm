#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The machine's tape has been weaved through a tree.

  We include the base tm as a slot, rather than inheriting it, because we need the
  specialized behavior of the tm methods inside the implementation of tm-depth methods.

|#

(in-package #:tm)


  ;; When stepping from a sublist, we step into the sublist to its first object.  We stack
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
  ;; possible to avoid this recursive unwinding by not stacking tm points on rightmost in
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
            (enqueue history (dup tm)) ; for later traversal from
            (si tm 
              cont-si
              (λ()
                (error 'tm-impossible-to-get-here 
                  :text "save-and-step-in is called after a consp check on tm"
                  ))
              ))

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
          (so tm cont-so #'dequeue-sublist)
          )))

    (defmethod s
      (
        (tm tm-depth)
        &optional
        (cont-ok (be t))
        (cont-rightmost (be ∅))
        )
      (labels(
               (step-depth()
                 (s-depth-ru
                   (tape tm)
                   (HA tm)
                   cont-ok
                   cont-ok
                   cont-rightmost
                   cont-ok
                   ))
               )
        (step-depth)
        ))


