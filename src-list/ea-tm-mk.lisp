#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Initializes the entanglements structure.  There can be a failure when
  allocating space for the entanglements list, so we pick up a cont-no-alloc
  for this version of init.

  Only one thread can be active with the instance during init an ea-tape-machine.  This is
  because we have init the entanglements slot, etc, and we must do this before any other
  thread uses the object.

|#

(in-package #:tm)

(defmethod init
  (
    (instance ea-tape-machine)
    init-list
    &optional 
    (cont-ok #'echo)
    (cont-fail (λ()(error 'bad-init-value)))
    &rest ⋯
    )
  (destructuring-bind
    (&optional (cont-no-alloc #'alloc-fail)) ⋯
    (mk 
      'list-solo-tm
      {:mount {instance}}
      (λ(ea-list)
        (setf (entanglements instance) ea-list)
        (call-next-method instance init-list cont-ok cont-fail)
        )
      cont-fail
      cont-no-alloc
      )))


