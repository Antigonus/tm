#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  A new tape machine may be made by calling mk.

  We have a circular definition problem with the entanglements list.
  We must be able to disentangle machines, so we will

|#

  ;; tm-type is tape-machine, or derived from tape-machine
  (defun mk (tm-type &rest init-list)
    (let(
          (instance (make-instance tm-type))
          )
      (setf (entanglements instance) ∅)
      (setf (adjoin instance (entanglements instance)))
      (init instance init-list)
      instance
      ))

  
