#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; copying
;;  

  ;; more specialized than one found in nd-tm-derived.lisp
  (defmethod with-mk-entangled
    (
      (tm ea-tape-machine)
      continuation
      )
    (let(
          (tm1 (mk-entangled tm))
          )
      (unwind-protect
        (funcall continuation tm1)
        (self-disentangle tm1)
        )))

  ;; more specialized than one found in nd-tm-primitives.lisp
  (defmethod init-entangled ((tm1 ea-tape-machine) tm-orig)
    (setf (entanglements tm1) (entanglements tm-orig))
    (self-entangle tm1) ; adds tm1 to the entanglement list
    (call-next-method tm1 tm-orig)
    )

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; add a new leftmost
  (defmethod a◧
    (
      (tm ea-tape-machine)
      object
      &optional
      (cont-ok  (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (call-next-method tm object cont-ok cont-no-alloc)
    (∀-entanglements-update-tape tm)
    )

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  (defmethod d (
                  (tm ea-tape-machine)
                  &optional 
                  spill 
                  (cont-ok #'echo)
                  (cont-rightmost (λ()(error 'dealloc-on-rightmost)))
                  (cont-no-alloc #'alloc-fail)
                  &rest ⋯
                  )
    (destructuring-bind
      (
        &optional
        (cont-collision (λ()(error 'dealloc-collision)))
        )
       ⋯
      (∃-collision-right-neighbor tm
        cont-collision
        (λ()(call-next-method tm spill cont-ok cont-rightmost cont-no-alloc))
        )))
      
  (defmethod d◧ (
                   (tm ea-tape-machine)
                   &optional 
                   spill 
                   (cont-ok #'echo)
                   (cont-collision (λ()(error 'dealloc-collision)))
                   (cont-no-alloc #'alloc-fail)
                   )
    (∃-collision◧ tm
      cont-collision
      (λ()
        (call-next-method tm spill cont-ok cont-collision cont-no-alloc)
        (∀-entanglements-update-tape tm)
        )))

    

