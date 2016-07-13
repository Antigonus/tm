#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r-0 ((tm solo-tm-list) (state active) cont-ok cont-parked)
    (declare (ignore cont-parked))
    (car (HA tm))
    (funcall cont-ok)
    )

  (defmethod w-0 ((tm solo-tm-list) (state active) object cont-ok cont-parked)
    (declare (ignore cont-parked))
    (setf (car (HA tm)) object)
    (funcall cont-ok)
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defmethod cue-leftmost-0 ((tm solo-tm-list) (state parked) cont-ok cont-void)
    (declare (ignore cont-void))
    (setf (HA tm) (tape tm))
    (setf (state tm) active)
    (funcall cont-ok)
    )
  (defmethod cue-leftmost-0 ((tm solo-tm-list) (state active) cont-ok cont-void)
    (declare (ignore cont-void))
    (setf (HA tm) (tape tm))
    (funcall cont-ok)
    )

;;--------------------------------------------------------------------------------
;; head location
;;
  (defmethod heads-on-same-cell-0
    (
      (tm0 solo-tm-list)
      (state0 active)
      (tm1 solo-tm-list)
      (state1 active) 
      cont-true
      cont-false
      cont-parked
      )
    (declare (ignore cont-parked))
    (if (eq (HA tm0) (HA tm1))
      (funcall cont-true)
      (funcall cont-false)
      ))

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defmethod s-0 ((tm solo-tm-list) (state active) cont-ok cont-rightmost)
    (if 
      (cdr (HA tm))
      (progn
        (setf (HA tm) (cdr (HA tm)))
        (funcall cont-ok)
        )
      (funcall cont-rightmost)
      ))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defmethod a-0 ((tm solo-tm-list) (state active) object cont-ok cont-no-alloc)
    (declare (ignore cont-no-alloc))
    (let(
          (next-cell (cdr (HA tm)))
          )
      (setf (HA tm) (cons object next-cell))
      (funcall cont-ok)
      ))

  (defmethod a◧-0 ((tm solo-tm-list) (state void) object cont-ok cont-no-alloc)
    (declare (ignore cont-no-alloc))
    (setf (tape tm) (cons object ∅))
    (setf (HA tm) (tape tm))
    (setf (state tm) parked)
    (funcall cont-ok)
    )
  (defmethod a◧-0 ((tm solo-tm-list) (state parked) object cont-ok cont-no-alloc)
    (declare (ignore cont-no-alloc))
    (setf (tape tm) (cons object (tape tm)))
    (funcall cont-ok)
    )
  (defmethod a◧-0 ((tm solo-tm-list) (state active) object cont-ok cont-no-alloc)
    (declare (ignore cont-no-alloc))
    (setf (tape tm) (cons object (tape tm)))
    (funcall cont-ok)
    )

;;--------------------------------------------------------------------------------
;; cell deallocation
;;

  ;; state is active when this is called
  ;; deallocates next-cell, spills and returns its contents
  (defmethod d-0 (tm spill cont-ok cont-rightmost cont-no-alloc)
    (declare (ignore cont-no-alloc))
    (if
      (cdr (HA tm))
      (let*(
             (dealloc-cell (cdr (HA tm)))
             (spill-object (car dealloc-cell))
             )
        (as spill spill-object
          (λ()
            (rplacd (HA tm) (cdr dealloc-cell))
            (funcall cont-ok spill-object)
            )
          cont-no-alloc
          ))
      (funcall cont-rightmost)
      ))

  ;; state is parked or active when this is called
  ;; deallocates cell at the leftmost of the tape
  (defmethod d◧-0 (tm spill cont-ok cont-no-alloc)
    (declare (ignore cont-no-alloc))
    (let*(
           (dealloc-cell (tape tm))
           (spill-object (car dealloc-cell))
           )
      (as spill spill-object
        (λ()
          (if
            (cdr (tape tm))
            (progn
              (setf (tape tm) (cdr dealloc-cell))
              (funcall cont-ok spill-object)
              )
            (progn
              (setf (state tm) void)
              (setf (HA tm) ∅)
              (setf (tape tm) ∅)
              )))
        cont-no-alloc
        )))
