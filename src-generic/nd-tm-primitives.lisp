#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Additional non-destructive primitives.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; copying
;;  
  (defgeneric mk-entangled (tm-orig)
    (:documentation
      "Make a new tape machine. Initializes the new machine by entangling it
       with tm-orig.  An entangled machine shares a tape, but has an independent
       head. Returns the new machine.
       "
      ))

  (defgeneric recycle-entangled (tm-to-be-recycled tm-orig)
    (:documentation
      "Like mk-entangled, but we recycle tm-to-be-recycled rather than creating
       a new instance.
       "
      ))

  (defgeneric mk-shallow-copy
    (
      tm-orig
      &optional
      cont-ok
      cont-no-alloc
      )
    (:documentation
      "Makes a new tape machine.  Initializes the tape with a copy of the tape found in
       tm-orig.  The new tape references the same objects as the tm-orig tape.  Because it
       has its own tape, the new machine is not entangled with the tm-orig machine.
       "
      ))

  ;; will work for most machines
  (defmethod mk-shallow-copy
    (
      (tm-orig tape-machine)
      &optional
      (cont-ok #'echo)
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (let(
          (tm-copy (make-instance (type-of tm-orig)))
          )
      (as* tm-copy tm-orig
        (λ()(funcall cont-ok tm-copy))
        cont-no-alloc
        )))

