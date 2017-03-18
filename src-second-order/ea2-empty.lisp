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
  ;; adding a cell to an empty machine will cause it to be parked, then stepping
  ;; one to the right will cause it to be active with the head on leftmost
  ;;
    (defun-typed as ((tm ea2-empty) instance &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (w (base tm) instance)
        (bt:with-lock-held ((lock (locked-entanglements tm)))
          (c◧∀* (entanglements (locked-entanglements tm)) (λ(es) (to-parked (r es))))
          )
        (to-active tm) ; state change is not head motion, we do not need to have the lock
        [➜ok]
        ))

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (defun-typed a◧ ((tm ea2-empty) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (w (base tm) instance)
      (bt:with-lock-held ((lock (locked-entanglements tm)))
        (c◧∀* (entanglements (locked-entanglements tm)) (λ(es) (to-parked (r es))))
        )
      [➜ok]
      ))
