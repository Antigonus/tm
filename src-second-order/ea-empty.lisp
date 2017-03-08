#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

We never make  empty machines, rather managed machines are change classed
to this type after a request to delete the last cell from the tape
belonging to a machine that has a parked head.

check that we correctly update addresses

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
    (defun-typed as ((tm ea-empty) instance &optional ➜)
      (destructuring-bind
        (&key
          (➜ok (be t))
          &allow-other-keys
          )
        ➜
        (w (base tm) instance)
        (cue-leftmost (entanglements tm))
        (∀* (entanglements tm) (λ(es) (to-parked (r es))))
        (to-active tm)
        [➜ok]
        ))

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
      (w (base tm) instance)
      (cue-leftmost (entanglements tm))
      (∀* (entanglements tm) (λ(es) (to-parked (r es))))
      [➜ok]
      ))
