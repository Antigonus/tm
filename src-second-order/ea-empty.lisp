#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

The only way to change states away from 'empty' is to add a new cell.


|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;

;;--------------------------------------------------------------------------------
;; tm-generic
;;

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm ea-empty) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (prins (print "a◧ ea-empty"))
      (w (base tm) instance)
      (c◧∀* (entanglements tm) (λ(es) (to-parked (tg:weak-pointer-value (r es)))))
      [➜ok]
      ))
