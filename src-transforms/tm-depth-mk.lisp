#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This machine's tape is woven in a depth first pattern through
  the base tm interpreted as a tree.

  The base machine passed in is forked.  Hence stepping here will
  not cause the base machine to step.  One can, however, cue-to
  this machine.  This was done because we might have multiple depth
  traversals going simultaneously on the same structure.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-depth (tape-machine)())

  (defstruct depth
    base ; machine being traversed
    history ; backtrack buffer
    )

  ;; base is another tape machine
  (defmethod init 
    (
      (tm tm-depth)
      init-list
      &optional 
      (cont-ok (be t))
      (cont-fail (λ()(error 'bad-init-value)))
      )
    (destructuring-bind
      (&key base buffer &allow-other-keys) init-list
      (cond
        ((∧ base buffer)
          (setf (parameters tm)
            (make-depth
              :base (fork base) ; caller might continue to use the base machine
              :history (make-instance 'stack :buffer buffer)
              ))
          (setf (state tm) active)
          (setf (head tm) (fork base))
          (setf (tape tm) ∅)
          (setf (entanglements tm) (make-entanglements tm))
          (funcall cont-ok)
          )
        (base
          (setf (parameters tm)
            (make-depth
              :base base
              :history (make-instance 'stack :buffer (mk 'tm-list))
              ))
          (setf (head tm) (fork base))
          (setf (tape tm) ∅)
          (setf (entanglements tm) (make-entanglements tm))
          (funcall cont-ok)
          )
        (t
          (funcall cont-fail)
          )
        )))
