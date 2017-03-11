#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list-tm machines. Typically the tm will be a list-nd-tm or a list-solo-tm
  instance, and then this gets called due to the inheritance structure.
  

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making list machines from other instances
;;
  (defun-typed init 
    (
      (tm list-tm)
      (keyed-parms cons)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys          
        )
      ➜
      (destructuring-bind
        (&key tape) keyed-parms
        (cond
          ((∧ tape (consp tape))
            (setf (head tm) tape)
            (setf (tape tm) tape)
            [➜ok tm]
            )
          (t
            (call-next-method keyed-parms ➜) ; pick up tape-machine's init for non consp tapes
            ))
        )))

