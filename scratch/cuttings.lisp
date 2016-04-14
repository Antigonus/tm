  ;; deallocate the leftmost cell
  (defmethod d◧
    (
      (tm tm-list)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )

    (let*(
           (dealloc-cell (tape tm)) ; first cell of tape
           (dealloc-object (car dealloc-cell))
           )

      (if (tm-list-singleton tm) 

        ;; collapse to void
        (progn
          (when spill
            (as spill dealloc-object 
              #'do-nothing 
              (λ()(return-from d◧ (funcall cont-no-alloc)))
              ))
          (change-class tm 'tm-void)
          (init tm {:tm-type 'tm-list}
            (λ() (funcall cont-ok dealloc-object))
            #'cant-happen
            ))

        ;;normal dealloc, not singleton, so has at least two cells
        (progn
          (w tm-leftmost (r-index tm-leftmost 1 #'do-nothing #'cant-happen))
          (d tm-leftmost spill cont-ok cont-no-alloc)
          )
        )))
