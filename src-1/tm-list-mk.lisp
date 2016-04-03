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
  (defmethod tm-init ((instance tm-list) init-list)
    (cond
      ((¬ init-list) 
        (let(
              (first-cell (cons 'list ∅))
              )
          (setf (tape instance) first-cell)
          (setf (HA instance) first-cell)
          instance
          ))

      ;; only one element, and that element is a list, then it is our list to bind to
      ((∧ (¬ (cdr init-list)) (consp (car init-list))) 
        (setf (tape instance) (car init-list))
        (setf (HA instance) (car init-list))
        instance
        )

      (t 
        (error 'tm-mk-bad-init-type)
        )
      ))

  (defmethod mount ((sequence cons) &optional (cont-ok #'echo) cont-fail)
    (declare (ignore cont-fail))
    (funcall cont-ok
      (make-instance 'tm-list :tape sequence :HA sequence)
      ))
