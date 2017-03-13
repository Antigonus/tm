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
      (init-parms cons)
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
        (&key tape &allow-other-keys) init-parms
        (cond
          ((∧ tape (typep tape 'sequence))
            (c◧ tm)
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

  (defun mk (tm-class init-parms &optional ➜)
    (let(
          (instance (make-instance tm-class))
          )
      (init instance init-parms ➜)
      ))





