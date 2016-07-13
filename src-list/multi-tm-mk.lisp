#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Initializes the entanglements structure.

|#

(in-package #:tm)

(defgeneric init-multi (instance init-list &optional cont-ok cont-fail))

(defmethod init
  (
    (instance multi-tape-machine)
    init-list
    &optional 
    cont-ok 
    cont-fail
    )
  (setf (entanglements instance) (mk 'solo-tm-list))

  ;; adds self to the entanglements list
  (a (entanglements instance) instance
    (λ()
      (init-multi instance init-list cont-ok cont-fail)
      )
    cont-fail
    ))

