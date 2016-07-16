#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Initializes the entanglements structure.

|#

(in-package #:tm)

(defmethod init
  (
    (instance multi-tape-machine)
    init-list
    &optional 
    cont-ok 
    cont-fail
    &rest
    ⋯
    )
  (destructuring-bind ⋯
    (
      (cont-no-alloc-entanglements (λ()(error 'alloc-fail)))
      )

    (setf (entanglements instance) (mk 'list-solo-tm))

    ;; adds self to the entanglements list
    (a (entanglements instance) instance #'do-nothing cont-no-alloc-entanglements) 
    (call-next-method)
    ))

