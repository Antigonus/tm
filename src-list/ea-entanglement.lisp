#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

The entanglements slot for ea-tm holds a list-solo-tm.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; adding and removing from the list
;;
  (defun self-disentangle (tm)
    "Removes tm from its own entanglement list.  This is typically done before 
    tm is abandoned.
    "
    (let(
          (es (entanglements tm))
          )
      (cue-leftmost es)
      (if (eq (r es) tm)
        (s es
          (λ()(d◧ es ∅ #'do-nothing #'cant-happen #'cant-happen))
          (setf (entanglements tm) ∅)
          )
        (∃ es
          (λ(es)
            (esr es
              (λ(object)
                (if (eq object tm)
                  (d es ∅ #'echo #'cant-happen)
                  ∅
                  ))
              (be ∅)
              ))))))


;;--------------------------------------------------------------------------------
;; detecting a collision
;;
  (defun collide (tm0 tm1 &optional (cont-true (be t)) (cont-false (be ∅)))
    "tm0 and tm1 are distinct machines, and have their heads on the same cell."
    (if
      (eq tm0 tm1)
      (funcall cont-false)
      (heads-on-same-cell tm0 tm1 cont-true cont-false)
      ))

  (defun ∃-collision (tm &optional (cont-true (be t)) (cont-false (be ∅)))
    "tm collides with an entangled machine."
    (let(
          (es (entanglements tm))
          )
      (if es
        (progn
          (cue-leftmost es)
          (∃ es (λ(es)(∧ (not (eq (r es) tm)) (heads-on-same-cell (r es) tm)))
            cont-true
            cont-false
            ))
        (funcall cont-false)
        )))

  (defun ∃-collision-right-neighbor (tm &optional (cont-true (be t)) (cont-false (be ∅)))
    "There exists in the entanglement list, a machine that has its head on
     tm's right neighbor.
    "
      (with-mk-entangled tm
        (λ(tms1)
          (s tms1
            (λ()(∃-collision tms1 cont-true cont-false))
            cont-false
            ))))
              
  (defun ∃-collision◧ (tm &optional (cont-true (be t)) (cont-false (be ∅)))
    "There exists in the entanglement list, a machine that has its head on leftmost."
    (let(
          (es (entanglements tm))
          )
      (if es
        (progn
          (cue-leftmost es)
          (∃ es (λ(es)(on-leftmost (r es)))
            cont-true
            cont-false
            ))
        (funcall cont-false)
        )))

;;--------------------------------------------------------------------------------
;; updating the tape
;;
  (defun ∀-entanglements-update-tape (tm)
    (let(
          (es (entanglements tm))
          )
      (cue-leftmost es)
      (∀ es (λ(es)(setf (tape (r es)) (tape tm)) t))
      ))
