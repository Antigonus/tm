
  (defgeneric d◧vs (
                   tm 
                   &optional 
                   spill 
                   cont-ok ; #'echo
                   cont-rightmost ; (λ()(error 'step-from-rightmost))
                   cont-no-alloc ; (λ()(error 'alloc-fail))
                   )
    (:documentation
      "Similar to d◧ with the following variation: if the head is on leftmost
       the head is stepped to the next cell.  If leftmost is also rightmost
       no action is taken then cont-rightmost.
       "
      ))

  (defmethod d◧vs
    (
      (tm list-solo-tm)
      &optional
      spill 
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'step-from-rightmost)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (if
      (cdr (tape tm))
      (let*(
             (dealloc-cell (tape tm))
             (spill-object (car dealloc-cell))
             )
        (when (eq (HA tm) (tape tm)) (setf (HA tm) (cdr tm)))
        (as spill spill-object
          (λ()
            (setf (tape tm) (cdr dealloc-cell))
            (funcall cont-ok spill-object)
            )
          cont-no-alloc
          ))
      (funcall cont-rightmost)
      ))
