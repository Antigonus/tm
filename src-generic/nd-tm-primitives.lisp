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
  (defgeneric recycle-entangled-with-0 (tm-recycle tm-orig tm-orig-state))

  (defmethod recycle-entangled-with-0 (tm-recycle (tm-orig nd-tape-machine) (tm-orig-state abandoned))
    (declare (ignore tm-recycle tm-orig tm-orig-state))
    (error 'operation-on-abandoned)
    )

  ;; this will work for many machines
  (defmethod recycle-entangled-with-0 (tm-recycle (tm-orig nd-tape-machine) tm-orig-state)
    (change-class tm-recycle (type-of tm-orig))
    (setf (state tm-recycle) (state tm-orig))
    (setf (HA tm-recycle) (HA tm-orig))
    (setf (tape tm-recycle) (tape tm-orig))
    (setf (parameters tm-recycle) (parameters tm-orig))
    tm-recycle
    )

  (defgeneric mk-entangled-with-0 (tm-orig tm-orig-state))

  (defmethod mk-entangled-with-0 ((tm-orig nd-tape-machine) (tm-orig-state abandoned))
    (error 'operation-on-abandoned)
    )

  (defmethod mk-entangled-with-0 ((tm-orig nd-tape-machine) tm-orig-state)
    (let(
          (instance (make-instance (type-of tm-orig)))
          )
      (entangle-0 instance tm-orig tm-orig-state)
      ))

