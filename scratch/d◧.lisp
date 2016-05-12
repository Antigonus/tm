  ;; if we are here, we are not void as tm-void has a more specific version than this
  ;; therefor leftmost exists
  ;;
    (defmethod d◧
      (
        (tm tape-machine)
        &optional 
        spill
        (cont-ok #'echo)
        (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
        (cont-not-supported (λ()(error 'dealloc-not-supported)))
        (cont-entangled (λ()(error 'dealloc-entangled)))
        (cont-no-alloc (λ()(error 'alloc-fail)))
        )

      (let(
            (tm&h◧ (dup tm))
            )
        (cue-leftmost tm&h◧)
        (let(
              (dealloc-object (r tm&h◧))
              )

          (if (singleton tm)

            (progn
              (when spill
                (as spill dealloc-object 
                  #'do-nothing 
                  (λ()(return-from d◧ (funcall cont-no-alloc)))
                  ))
              (void tm)
              )

            ;;not singleton, so has at least two cells
            ;; swap objects first two cells, then call #'d to dealloc second
            (progn
              (let(
                    (tm&h◧s (dup tm&h◧))
                    )
                (s tm&h◧s #'do-nothing #'cant-happen)
                (let(
                      (keep-object (r tm&h◧s))
                      )
                  (w tm&h◧ keep-object)
                  (w tm&h◧s dealloc-object)
                  (d tm&h◧ spill
                    cont-ok
                    cont-rightmost
                    cont-not-supported
                    cont-entangled
                    cont-no-alloc
                    )
                  )))
            ))))
                
