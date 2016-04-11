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
  (defclass tm-singular (tm-void)())

  (defmethod init 
    (
      (tm tm-singular)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
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
      (cont-no-alloc (λ()(error 'tm-alloc-fail)))
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

  (defmethod d 
    (
      (tm tm-singular)
      &optional 
      spill
      cont-ok
      (cont-no-dealloc (λ()(error 'dealloc-fail)))
      (cont-no-alloc  (λ()(error 'alloc-fail)))
      )
    (declare (ignore cont-no-dealloc))
    (when spill 
      (as spill (tape tm) #'do-nothing (λ()(return-from d (funcall cont-no-alloc))))
      )
    (setf (tape tm) ∅)
    (change-class tm 'tm-void)
    (funcall cont-ok)
    )
