#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Functions derived through quantification.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; repeated until end of tape operations
;;
   (def-function-class d* (tm &optional spill ➜)
     (:documentation
       "Deallocates all cells right of the head up to and including rightmost.
       If spill is not ∅, then the deallocated right side cells are moved to it.
       Preferably the cells are moved, but it is premissable for an implementation to
       create a new allocation on spill and then copy contents.
      "
     ))

   (defun-typed d*
     (
       (tm solo-tape-machine)
       &optional spill ➜
       )
     (destructuring-bind
       (&key
         (➜ok (be t))
         (➜no-alloc #'alloc-fail)
         &allow-other-keys
         )
       ➜
       (⟳(λ(again)
           (d tm spill
             {
               :➜ok (λ(instance)(declare (ignore instance)) [again])
               :➜rightmost ➜ok
               :➜no-alloc ➜no-alloc
               })))
       ))

   (def-function-class d◧* (tm &optional spill ➜)
     (:documentation
       "Deallocates leftmost, repeatedly, until colliding with the cell the head is on,
        which is not deallocated.  If spill is not ∅, then the deallocated cells are moved
        to it.  Preferably the cells are moved, but it is premissable for an
        implementation to create a new allocations on spill and then copy the instance
        references.
        "
        ))

   ;; pacman the tape until colliding with the cell the head is on (guaranteed to happen)
   (defun-typed d◧*
     (
       (tm solo-tape-machine)
       &optional spill ➜
       )
     (destructuring-bind
       (&key
         (➜collision (be t)) ; true, we hit the cell the head is on, we are done
         (➜no-alloc #'alloc-fail)
         &allow-other-keys
         )
       ➜
       (⟳(λ(again)
           (d◧ tm spill
             {
               :➜ok (λ(instance)(declare (ignore instance))[again])
               :➜no-alloc ➜no-alloc
               :➜collision ➜collision ; can't delete a cell the head is on
               })))
       ))

;;--------------------------------------------------------------------------------
;; repeated by count operations
;;
  (def-function-class dn (tm count &optional spill ➜)
    (:documentation
      "Given a tape machine and a natural number.
      Like repeating d count times, but specialized versions might be more efficient.
      "
      ))

  (defun-typed dn
    (
      (tm solo-tape-machine)
      (n integer)
      &optional spill ➜
      )
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (labels(
               (do-work()
                 (when (≤ n 0) (return-from dn [➜ok]))
                 (d tm spill
                   {
                     :➜ok (λ(instance)
                            (declare (ignore instance))
                            (decf n)
                            (do-work)
                            )
                     :➜rightmost (λ()(return-from dn [➜rightmost n]))
                     :➜no-alloc ➜no-alloc
                     })
                 ))
        (do-work)
        )))
