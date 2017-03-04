#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|#

;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (defun-typed cue-leftmost ((tm status-parked) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (change-class tm 'status-active)
      [➜ok]
      ))


;;--------------------------------------------------------------------------------
;;tm-generic
;;
  (defun-typed cue-rightmost ((tm status-parked) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (cue-rightmost (base tm)
        {
          :➜ok (λ()(change-class tm 'status-active) [➜ok])
          }
        )))

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;

  ;; must cue-leftmost all in entanglement group
  ;; must update addresses
  (defun-typed a◧ ((tm status-parked) instance &optional ➜) (a◧ (base tm) instance ➜))


  (defun-typed d ((tm status-parked) &optional spill ➜) (d◧ (base tm) spill ➜))
  (defun-typed d◧ ((tm status-parked) &optional spill ➜) (d◧ (base tm) spill ➜))
