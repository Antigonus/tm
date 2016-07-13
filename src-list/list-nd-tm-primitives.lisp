;;--------------------------------------------------------------------------------
;; head location
;;
  (defmethod heads-on-same-cell
    (
      (tm0 list-tm)
      (tm1 list-tm)
      &optional
      cont-true
      cont-false
      &rest ⋯
      )
    (declare (ignore ⋯)) 
    (if (eq (HA tm0) (HA tm1))
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod heads-on-same-cell
    (
      (tm0 list-tm)
      tm1
      &optional
      cont-true
      cont-false
      &rest ⋯
      )
    (declare (ignore cont-true ⋯)) 
    (funcall cont-false)
    )

  (defmethod heads-on-same-cell
    (
      tm0
      (tm1 list-tm)
      &optional
      cont-true
      cont-false
      &rest ⋯
      )
    (declare (ignore cont-true ⋯)) 
    (funcall cont-false)
    )
