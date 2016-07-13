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

   (defgeneric d-1
     (
       tm
       tm-state
       spill 
       cont-ok 
       cont-rightmost
       cont-collision
       cont-no-alloc
       )
     )

   (defmethod d-1 
     (
       (tm solo-tape-machine)
       (tm-state abandoned)
       spill 
       cont-ok 
       cont-rightmost
       cont-collision
       cont-no-alloc
       )
     (declare (ignore tm spill cont-ok cont-rightmost cont-collision cont-no-alloc))
     (error 'operation-on-abandoned)
     )

   (defmethod d-1 
     (
       (tm solo-tape-machine)
       (tm-state void)
       spill 
       cont-ok 
       cont-rightmost
       cont-collision
       cont-no-alloc
       )
     ;; here we see void as the limiting case of deleting leftmost from a parked machine
     ;; rather than duplicating that code we call d◧
     (d◧ tm spill cont-ok cont-rightmost cont-collision cont-void cont-no-alloc)
     )

   (defmethod d-1 
     (
       (tm solo-tape-machine)
       (tm-state parked)
       spill 
       cont-ok 
       cont-rightmost
       cont-collision
       cont-no-alloc
       )
     (d◧-1 tm parked spill cont-ok cont-rightmost cont-collision cont-void cont-no-alloc)
     )

   (defmethod d-1 
     (
       (tm solo-tape-machine)
       (tm-state active)
       spill 
       cont-ok 
       cont-rightmost
       cont-collision
       cont-no-alloc
       )
     ;; solo machine can't collide with another machine
     (declare (ignore cont-collision)) 
     (d-0 tm spill cont-ok cont-rightmost cont-no-alloc)
     )
  

  (defun d◧ (
              tm 
              &optional 
              spill 
              (cont-ok #'echo)
              (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
              (cont-collision (λ()(error 'dealloc-entangled)))
              (cont-no-alloc (λ()(error 'alloc-fail)))
              )
    "The leftmost cell is deallocated independent of where the head is located."
    (d◧-1 tm (state tm) spill cont-ok cont-rightmost cont-collision cont-no-alloc)
    )

   (defgeneric d◧-1
     (
       tm
       tm-state
       spill 
       cont-ok 
       cont-rightmost
       cont-collision
       cont-no-alloc
       )
     )

   (defmethod d◧-1 
     (
       (tm solo-tape-machine)
       (tm-state abandoned)
       spill 
       cont-ok 
       cont-rightmost
       cont-collision
       cont-no-alloc
       )
     (declare (ignore tm spill cont-ok cont-rightmost cont-collision cont-no-alloc))
     (error 'operation-on-abandoned)
     )

   (defmethod d◧-1 
     (
       (tm solo-tape-machine)
       (tm-state void)
       spill 
       cont-ok 
       cont-rightmost
       cont-collision
       cont-no-alloc
       )
     (declare (ignore tm spill cont-ok cont-rightmost cont-collision cont-no-alloc))
     (funcall cont-rightmost) ;; limiting case of d-1 called from parked
     )

   (defmethod d◧-1 
     (
       (tm solo-tape-machine)
       (tm-state parked)
       spill 
       cont-ok 
       cont-rightmost
       cont-collision
       cont-no-alloc
       )
     (declare (ignore cont-rightmost cont-collision))
     (d◧-0 tm spill cont-ok cont-no-alloc)
     )

   (defmethod d◧-1 
     (
       (tm solo-tape-machine)
       (tm-state active)
       spill 
       cont-ok 
       cont-rightmost
       cont-collision
       cont-no-alloc
       )
     (declare (ignore cont-rightmost))
     (if
       (on-leftmost tm)
       (funcall cont-collision)
       (d◧-0 tm spill cont-ok cont-no-alloc)
       ))
