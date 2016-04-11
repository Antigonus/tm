#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

(HA tm) holds the type of the tape space
(tape tm) holds an object

Calling alloc, #'a, transitions into the tape space.
Calling dealloc, #'d transitions to 'tm-void.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-singular (tm-parked-singular)())

  (defmethod init 
    (
      (tm tm-singular)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (error 'bad-init-value))
      )
    (destructuring-bind
      (&key tape-space mount &allow-other-keys) init-list
      (unless 
        (∧
          tape-space
          mount
          (= (length mount) 1)
          )
        (return-from init (funcall cont-fail))
        )
      (setf (HA tm) tape-space)
      (setf (tape tm) (car mount))
      (funcall cont-ok)
      ))


;;--------------------------------------------------------------------------------
;; primitive methods
;;
  (defmethod r ((tm tm-singular)) (tape tm))
  (defmethod w ((tm tm-singular) object) (setf (tape tm) object))

  ;; transitions into the tape space
  (defmethod a
    (
      (tm tm-singular)
      object-1
      &optional
      cont-ok
      (cont-no-alloc (error 'tm-alloc-fail))
      )
    (let(
          (tm1 (dup tm))
          (object-0 (tape tm))
          )
      (change-class tm1 (HA tm))
      (init tm1 :mount {object-0 object-1}
        (λ()
          (cue-to tm tm1)
          (funcall cont-ok)
          )
        cont-no-alloc
        )))

