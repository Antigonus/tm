#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; make a tm-entangled machine that shares the tape with tm-orig
;;
  (defun-typed entangle ((tm-orig list-nd-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        ;; (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜  
      (let(
            (tm-entangled (make-instance (type-of tm-orig)))
            )
        (setf (head tm-entangled) (head tm-orig))
        (setf (tape tm-entangled) (tape tm-orig))
        [➜ok tm-entangled]
        )))


    
