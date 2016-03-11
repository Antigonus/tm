
  (defmethod d◧ 
    (
      (tm tm-subspace)
      &optional 
      spill
      (cont-ok  (be t))
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (if
      (heads-on-same-cell (subspace-leftmost (tape tm)) (subspace-rightmost (tape tm)))
      (funcall cont-rightmost)
      (progn
        (when ; if the head is on leftmost, we bump it right one
          (heads-on-same-cell (subspace-leftmost (tape tm)) (HA tm))
          (s (HA tm))
          )
        (a spill (r tm) 
          (λ()
            (s (subspace-leftmost (tape tm)) ; to dealloc subspace's leftmost, just step it
              cont-ok 
              (λ()(error 'tm-impossible-to-get-here) :text "already did a rightmost check")
              ))
          cont-no-alloc
          ))))
