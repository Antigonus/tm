
  ;; programmer promises that tm is on rightmost
  (defmethod ah◨ ((tm tm-list) object)
    (let(
          (new-cell (cons object ∅))
          )
      (rplacd (HA tm) new-cell)
      (setf (HA tm) new-cell) ; moves head right, to the new cell
      t
      ))

  (defmethod ah◨. ((tm tm-list) object)
    (let(
          (new-cell (cons object ∅))
          )
      (rplacd (HA tm) new-cell)
      t
      ))

  (defun test-ah◨-10 ()
    (let*(
           (a (list 1 2 3))
           (tm1 (mk-tm-list-0 a))
           )
      (cue-rightmost tm1)
      (ah◨ tm1 '4)
      (equal
        (to-list tm1)
        '(1 2 3 4)
        )))
  (test-hook test-ah◨-10)

  ;; programmer promises that tm is on leftmost
  ;; tape r/w head moves left to be on the new cell
  (defmethod -ah◧ ((tm tm-list) object)
    (let(
          (new-cell (cons object ∅))
          )
      (rplacd new-cell (HA tm))
      (setf (HA tm) new-cell) 
      t
      ))
  ;; deallocates the leftmost cell, if the head is on leftmost, moves it to the
  ;; new leftmost
  (defmethod d◧ 
    (
      (tm tm-list)
      &optional 
      (spill 'd)
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      )

    (unless ; can't delete lm if it is the only cell
      (cdr (tape tm))
      (return-from d◧ (funcall cont-rightmost))
      )

    (when ; if head is on lm, step it
      (eq (cdr (tape tm)) (cdr (HA tm)))
      (setf (HA tm) (cdr (HA tm)))
      )

    
    (let(
          (de-lm (tape tm)) ; what we store as 'tape is in fact lm
          )
      (setf (tape tm) (cdr (tape tm))) ; this deallocates de-lm

       ;; now that de-lm has been deallocated from the tape, what to do with it?
      (cond
        ((eq spill 'd) (funcall cont-ok))
        ((eq spill 'r) (funcall cont-ok de-lm))
        ((typep spill 'tape-machine)
          (gs-list spill de-lm)
          (funcall cont-ok)
          )
        (t
          (error
            'tm-deallocate-bad-spill-command
            :text "allowed spill commands are: 'd 'r and (typep tape-machine)"
            )))
      ))

