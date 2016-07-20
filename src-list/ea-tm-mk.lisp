#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Initializes the entanglements structure.

|#

(in-package #:tm)

(defmethod init
  (
    (instance ea-tape-machine)
    init-list
    &optional 
    cont-ok 
    cont-fail
    &rest ⋯
    )
  (destructuring-bind
    (
      &optional
      (cont-no-alloc-entanglements (λ()(error 'alloc-fail)))
      )
    ⋯
    (setf (entanglements instance) (mk 'list-solo-tm))
    (entangle instance
      (λ()(call-next-method instance int-list cont-ok cont-fail))
      cont-no-alloc-entanglements
      )))

