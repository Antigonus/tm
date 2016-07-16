#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

We know that entanglements is a solo-tm

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; are two machines entangled?
;;
  ;; entanglement is mutual
` (defun are-entangled (tm0 tm1 &optional (cont-true (be t)) (cont-false (be ∅)))
    (∃ (entanglements tm1) (λ(es)(eq (r es) tm0)))
    )

;;--------------------------------------------------------------------------------
;; detecting a collision
;;
  (defun collision (tm0 tm1 &optional (cont-true (be t)) (cont-false (be ∅)))
    "tm0 and tm1 are distinct machines, and have their heads on the same cell."
    (if
      (∧
        (¬ (eq tm0 tm1))
        (heads-on-same-cell tm0 tm1)
        )
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defun ∃-collision-0 (tm es &optional (cont-true (be t)) (cont-false (be ∅)))
    "tm collides with one of the machines listed on es."
    (unless es (return-from ∃-collision-0 (funcall cont-false)))
    (cue-leftmost es)
    (∃ es (λ(e)(collision tm (r e)))
      cont-true
      cont-false
      ))

  (defun ∃-collision (tm &optional (cont-true (be t)) (cont-false (be ∅)))
    "tm collides with an entangled machine."
    (let(
          (es (entanglements tm))
          )
      (∃-collision-0 tm es cont-true cont-false)
      ))

  (defun ∃-collision-right (tm &optional (cont-true (be t)) (cont-false (be ∅)))
    "an entangled machine has its head on a cell to the right of the cell tm's head is on
    "
    (let(
          (es (entanglements tm))
          (tm0 (fork-0 tm))
          )
      (unless es (return-from ∃-collision-right (funcall cont-false)))
      (∃-collision-0 tm es
        cont-true
        (λ()
          (s tm0  ; after step tm0 will not collide with tm
            (λ()
              (⟳-loop
                (λ(cont-loop)
                  (∃-collision-0 tm0 es
                    cont-true
                    (λ()(s tm0 cont-loop cont-false))
                    ))))
            cont-false
            )))))

  (defun ∃-collision◧ (tm &optional (cont-true (be t)) (cont-false (be ∅)))
    "an entangled machine is on leftmost."
    (let(
          (es (entanglements tm))
          )
      (unless es (return-from ∃-collision◧ (funcall cont-false)))
      (cue-leftmost es)
      (∃ es (λ(e)(on-leftmost (r e)))
        cont-true
        cont-false
        )))

