#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; entanglements support
;;

  ;machine in entanglement group is on -s* ?
  (defun entangled-on-leftmost (es &optional (➜t (be t)) (➜∅ (be ∅)))
    (-s*∃ es
      (λ(es ct c∅)
        (if
          (∧
            (typep (r es) 'active)
            (= (address (r es)) 0)
            )
          [ct]
          [c∅]
          ))
      {
        :➜t ➜t
        :➜∅ ➜∅
        }
      ))

  ;another machine in entanglement group on same cell as tm?
  (def-function-class entangled-on-same-cell (entanglements-tm &optional ➜))
  (def-function-class entangled-on-right-neighbor-cell (entanglements-tm &optional ➜))


;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;


