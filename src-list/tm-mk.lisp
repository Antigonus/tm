#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; initialize a tape machine of the specified class to hold the specified instances
;;
  (def-function-class init (instance init-value &optional cont-ok cont-fail &rest ⋯))

  (defun-typed init 
    (
      (tm tape-machine)
      init-value
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      &rest ⋯
      )
    (declare (ignore ⋯ cont-ok))
    [cont-fail]
    )

  (defun mk
    (
      tm-class
      init-value
      &optional
      (cont-ok #'echo)
      (cont-fail (λ()(error 'bad-init-value)))
      (cont-no-alloc #'alloc-fail))
    (declare (ignore cont-no-alloc)) ; need to fix this
    (let(
          (instance (make-instance tm-class))
          )
      (init instance init-value cont-ok cont-fail)
      ))

;;--------------------------------------------------------------------------------
;; copying
;;  
  (def-function-class mk-shallow-copy
    (
      tm-orig
      &optional
      cont-ok
      cont-no-alloc
      )
    (:documentation
      "Makes a new tape machine.  Initializes the tape with a copy of the tape found in
       tm-orig.  The new tape references the same instances as the tm-orig tape.  Because it
       has its own tape, the new machine is not entangled with the tm-orig machine.
       "
      ))

  ;; will work for most machines
  (defun-typed mk-shallow-copy
    (
      (tm-orig tape-machine)
      &optional
      (cont-ok #'echo)
      (cont-no-alloc #'alloc-fail)
      )
    (let(
          (tm-copy (make-instance (type-of tm-orig)))
          )
      (as* tm-copy tm-orig
        (λ()[cont-ok tm-copy])
        cont-no-alloc
        )))



