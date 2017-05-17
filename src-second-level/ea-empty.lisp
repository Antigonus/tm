#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

The only way to change states away from 'empty' is to add a new cell.


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; entanglements support
;;
  ;; can't be entangeled on the same cell, when not on any cell at all
  (defun-typed entangled-on-same-cell ((tm ea-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  ;; if one entangled machine is empty, then all are empty
  (defun-typed entangled-on-right-neighbor-cell ((tm ea-empty) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

;;--------------------------------------------------------------------------------
;; tm-decl-only
;;

;;--------------------------------------------------------------------------------
;; tm-generic
;;

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;

  ;; for an empty machine address and address-rightmost are already 0
  (defun-typed epa ((tm ea-empty) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      ;; (prins (print "epa ea-empty"))
      (w (base tm) instance)
      (◧∀* (entanglements tm)
        (λ(es) 
          (let(
                (etm (r es))
                )
            (when etm (to-parked etm))
            )))
      [➜ok]
      ))
