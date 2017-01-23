#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; initialize a tape machine of the specified class to hold the specified instances
;;
  (def-function-class init (instance init-value &optional ➜))

  ;; init will throw an error that the function #'w is not found if tape-machine
  ;; does not have an implementation
  (defun-typed init
    (
      (tm tape-machine)
      (keyed-parms cons)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (destructuring-bind
        (&key tape &allow-other-keys) keyed-parms
        (cond
          ((∧ tape (typep tape 'sequence))
            (cue-leftmost tm)
            (w tm (elt tape 0))
            (loop
              for item in (subseq tape 1) do
              (as tm item {:➜ok #'do-nothing :➜no-alloc ➜no-alloc})
              )
            [➜ok tm]
            )
          (tape [➜fail]) 
          (t [➜ok tm]) ; there is no obligation to provide an initialization sequence
          ))))

  (defun mk (tm-class keyed-parms &optional ➜)
    (let(
          (instance (make-instance tm-class))
          )
      (init instance keyed-parms ➜)
      ))

;;--------------------------------------------------------------------------------
;; copying
;;  
  (def-function-class mk-shallow-copy (tm-orig &optional ➜)
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
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (let(
            (tm-copy (make-instance (type-of tm-orig)))
            )
        (as* tm-copy tm-orig
          {
            :➜ok (λ()[➜ok tm-copy])
            :➜no-alloc ➜no-alloc
            }))))



