#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This machine's tape is woven in a depth first pattern through
  the base tm interpreted as a tree.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-breadth (tape-machine)())

  (defmethod tm-init
    (
      (instance tm-breadth)
      &optional 
      init
      (cont-ok #'echo) 
      (cont-fail 
        (λ() (error 'tm-mk-init-failed :text "unrecognized list tape type"))
        ))
    (cond
      ((¬ init) ; user ∅ or default, will be based on an 'tm-list of one cell
        (setf (tape instance) (tm-mk 'tm-list))
        (setf (HA instance) (tm-mk 'queue-list))
        (funcall cont-ok instance)
        )

      ((typep init 'tape-machine)
        (setf (tape instance) init)
        (setf (HA instance) (tm-mk 'queue-list))
        (funcall cont-ok instance)
        )

      ((∧ (consp init) (cdr init) (¬ (cddr init)))
        (setf (tape instance) (car init))
        (setf (HA instance) (cadr init))
        (funcall cont-ok instance)
        )

      (t
        (funcall cont-fail)
        )))

