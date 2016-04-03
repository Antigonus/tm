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
  (defclass tm-depth (tape-machine)())

  (defmethod tm-init ((instance tm-depth) init-list)
    (cond
      ((¬ init-list) ; user ∅ or default, will be based on an 'tm-list of one cell
        (setf (tape instance) (tm-mk 'tm-list))
        (setf (HA instance) (tm-mk 'stack-list))
        instance
        )

      ;; only one item, and that item is a tm, then it is our tm to bind to
      ((∧ (¬ (cdr init-list)) (typep (car init-list) 'tape-machine))
        (setf (tape instance) (car init-list))
        (setf (HA instance) (tm-mk 'stack-list))
        instance
        )

      (t
        (error 'tm-mk-bad-init-type)
        )))

