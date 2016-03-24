#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines from other objects.
  Make other objects from list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;

  (defclass tm-list (tape-machine)())

;;--------------------------------------------------------------------------------
;; making tm-list machines from other objects
;;

  (defmethod tm-init
    (
      (instance 'tm-mk-list)
      &optional 
      init
      (cont-ok #'echo) 
      (cont-fail 
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized list tape type"))
        ))
    (cond
      ((∨ (¬ init) (eq (type-of init) 'tm-list)); user ∅ or default, goes to a meta list first cell
        (let(
              (first-cell (cons 'list ∅))
              )
          (setf (tape instance) first-cell)
          (setf (HA instance) first-cell)
          (funcall cont-ok instance)
          ))

      ((typep init 'cons)
        (setf (tape instance) init)
        (setf (HA instance) init)
        (funcall cont-ok instance)
        )

      (t
        (funcall cont-fail)
        )))

  (defmethod mount
    (
      (sequence 'cons) 
      &optional
      (cont-ok #'echo)
      (cont-fail 
        (λ() (error 'tm-mk-init-failed :text "unrecognized list tape type"))
        ))
    (let(
          (instance (make-instance 'tm-list))
          )
      (tm-init instance sequence cont-ok cont-fail)
      ))
