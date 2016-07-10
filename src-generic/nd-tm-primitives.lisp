#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Additional non-destructive primitives.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; copying
;;  
  (defgeneric entangle-0 (tm-recycled tm-orig tm-orig-state))

  (defmethod entangle-0 (tm-recycled (tm-orig nd-tape-machine) (tm-orig-state abandoned))
    (declare (ignore tm-recycled tm-orig tm-orig-state))
    (error 'operation-on-abandoned)
    )

  ;; this will work for many machines
  (defmethod entangle-0 (tm-recycled (tm-orig nd-tape-machine) tm-orig-state)
    (change-class tm-recycled (type-of tm-orig))
    (setf (state tm-recycled) (state tm-orig))
    (setf (HA tm-recycled) (HA tm-orig))
    (setf (tape tm-recycled) (tape tm-orig))
    (setf (parameters tm-recycled) (parameters tm-orig))
    tm-recycled
    )

  (defgeneric mk-entangled-0 (tm-orig tm-orig-state))

  (defmethod mk-entangled-0 ((tm-orig nd-tape-machine) (tm-orig-state abandoned))
    (error 'operation-on-abandoned)
    )

  (defmethod mk-entangled-0 ((tm-orig nd-tape-machine) tm-orig-state)
    (let(
          (instance (make-instance (type-of tm-orig)))
          )
      (entangle-0 instance tm-orig tm-orig-state)
      ))

