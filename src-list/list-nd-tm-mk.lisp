#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making list machines from other instances
;;

  ;; the init-value as a cons cell falls back to list-tm version.

  ;; This makes an entangled machine.  It is safe to do so as nd machines
  ;; do not support destructive operations.
  ;;
    (defun-typed init 
      (
        (tm1 list-nd-tm)
        (tm0 list-nd-tm) ; make an entangled copy of tm0
        &optional ➜
        )
      (destructuring-bind
        (&key
          (➜ok #'echo)
          &allow-other-keys          
          )
        ➜
        (setf (head tm1) (head tm0))
        (setf (tape tm1) (tape tm0))
        [➜ok tm1]
        ))
    
