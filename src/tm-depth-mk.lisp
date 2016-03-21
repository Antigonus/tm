#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This machine's tape is woven in a depth first pattern through
  the base tm interpreted as a tree.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;  tape machine follows a depth first traversal of a tree
;;
  (defclass tm-depth (tm-tape-machine)())

  (defun mk-tm-depth
    (
      &optional 
      init
      (cont-ok #'echo) 
      (cont-fail 
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized list tape type"))
        ))

    (cond
      ((¬ init) ; user ∅ or default, will be based on an 'tm-list of one cell
        (setf (tape instance) (mk-tm 'tm-list))
        (setf (HA instance) (mk-stack-list))
        (funcall cont-ok instance)
        )

      ((typep init 'tape-machine)
        (setf (tape instance) init)
        (setf (HA instance) (mk-stack-list))
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

  (mk-tm-hook 'tm-depth #'mk-tm-depth)

;;--------------------------------------------------------------------------------
;; making other objects from tm-list machines
;;
  ;; this returns list of the current subspace that the machine is in
  ;; to convert the entire space, apply to-list to the original base tm instead
  (defmethod to-list ((tm tm-depth)) (to-list (tape tm)))
  
