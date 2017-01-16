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
  (defun-typed with-mk-entangled
    (
      (tm0 ea-tape-machine)
      continuation
      )
    (let(
          (tm1 (mk (type-of tm0) tm0))
          )
      (unwind-protect
        (funcall continuation tm1)
        (self-disentangle tm1)
        )))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; add a new leftmost
  (defun-typed a◧
    (
      (tm ea-tape-machine)
      instance
      &optional
      (cont-ok  (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (call-next-method tm instance cont-ok cont-no-alloc)
    (∀-entanglements-update-tape tm)
    )

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  (defun-typed d
    (
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
        &key
        (cont-collision (λ()(error 'dealloc-collision)))
        )
      ⋯
      (∃-collision-right-neighbor tm cont-collision call-next-method)
      ))

;        (λ()(call-next-method tm spill cont-ok cont-rightmost cont-no-alloc))

      
  (defun-typed d◧
    (
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

    

