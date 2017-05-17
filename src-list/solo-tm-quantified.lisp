#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; cell deallocation
;;
  (def-function-class d* (tm &optional spill ➜)
    (:documentation
      "Deallocate the right hand side of the tape."
      ))

   ;;; It doesn't make sense to have d◧*, (which would be the same as ehpd* for a status
   ;;; machine) because it results in a collision in all cases except when the head is on
   ;;; leftmost. Instead do h◧ followed by d*

  (def-function-class dn (tm n &optional spill ➜)
    (:documentation
      "Deallocate rightmost n times, or take a rightmost continuation.  The remaining
       n is passed to the continutation"
      ))

  ;; generic implementation
  ;; this is a solo machine, so we can't have a collision
  (defun-typed d* ((tm solo-tape-machine) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜rightmost (be t))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (⟳ (λ(repeat)
           (d tm spill
             {
               :➜ok (λ(instance)(declare (ignore instance))[repeat])
               :➜rightmost ➜rightmost
               :➜no-alloc ➜no-alloc
               })))
      ))

  ;; this is a solo machine, so we can't have a collision
  (defun-typed dn ((tm solo-tape-machine) (n number) &optional spill ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost #'echo)
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (if (> n 0)
        (⟳ (λ(repeat)
             (d tm spill
               {
                 :➜ok (λ(instance)(declare (ignore instance))
                        (decf n)
                        (if (> n 0)
                          [repeat]
                          [➜ok]
                          ))
                 :➜rightmost (λ()[➜rightmost n])
                 :➜no-alloc ➜no-alloc
                 })))
        [➜ok]
        )))

;;--------------------------------------------------------------------------------
;; filtering
;;
  ;; filtering is just conditional d*
  ;; first filtered cell is the right neighbor to the cell the head is currently on
  ;; to affect the whole for first level machines, put the leftmost test in a priming loop
  ;; to affect the whole tape while using a status machine, first park it, 
  ;; when spill is ∅, the spilled cells are deallocated
  (def-function-class filter (tm spill pred &optional ➜))

  (defun-typed filter ((tm solo-tape-machine) spill pred &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (⟳
        (λ(➜again)
          (if (on-rightmost tm)
            [➜ok]
            [pred tm
              (λ()(d tm spill
                    {
                      :➜ok (λ(instance)(declare (ignore instance))[➜again])
                      :➜rightmost #'cant-happen
                      :➜no-alloc ➜no-alloc
                      }))
              (λ() (s tm
                     {
                       :➜ok ➜again
                       :➜rightmost #'cant-happen
                       }))
              ])))))

