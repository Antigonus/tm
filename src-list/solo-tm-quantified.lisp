#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Functions derived through quantification.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; repeated until end of tape operations
;;
   (defgeneric d*
     (
       tm
       &optional 
       spill 
       cont-ok
       cont-no-alloc
       )
     (:documentation
       "Deallocates all cells right of the head up to and including rightmost.
       If spill is not ∅, then the deallocated right side cells are moved to it.
       Preferably the cells are moved, but it is premissable for an implementation to
       create a new allocation on spill and then copy contents.
      "
     ))

   (defmethod d*
     (
       tm
       &optional 
       spill 
       (cont-ok (be t))
       (cont-no-alloc (λ()(error 'alloc-fail)))
       )
     (⟳-loop(λ(cont-loop)
       (d
         tm
         spill
         cont-loop
         cont-ok
         cont-no-alloc
         ))))

   (defgeneric d◧*
     (
       tm
       &optional 
       spill 
       cont-collision
       cont-no-alloc
       &rest
       ⋯
       )
     (:documentation
       "Deallocates leftmost, repeatedly, until colliding with the cell the head is on,
        which is not deallocated.  If spill is not ∅, then the deallocated cells are moved
        to it.  Preferably the cells are moved, but it is premissable for an
        implementation to create a new allocations on spill and then copy the object
        references.
        "
        ))

   (defmethod d◧*
     (
       tm
       &optional 
       spill 
       (cont-collision (be t))
       (cont-no-alloc (λ()(error 'alloc-fail)))
       &rest
       ⋯
       )
     (declare (ignore ⋯))
     (⟳-loop(λ(cont-loop)
       (d◧
         tm
         spill
         cont-loop
         cont-collision ; cont-collision - hit the cell the head was on
         cont-no-alloc
         ))))

;;--------------------------------------------------------------------------------
;; repeated by count operations
;;
  (defgeneric dn (tm count &optional spill cont-ok cont-rightmost)
    (:documentation
      "Given a tape machine and a natural number.
      Like repeating d count times, but specialized versions might be more efficient.
      "
      ))

  (defmethod dn
    (
      (tm tape-machine)
      (n integer)
      &optional 
      spill
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (labels(
             (do-work()
               (when (≤ n 0) (return-from dn (funcall cont-ok)))
               (d tm spill 
                 #'do-work
                 (λ()(return-from dn (funcall cont-rightmost n)))
                 )
               (decf n)
               )
             )
      (do-work)
      ))
