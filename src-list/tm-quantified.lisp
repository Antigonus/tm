#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Quantified operations.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; repeated until end of tape operations
;;   more specific versions, if they exist, are surely more efficient
;;
  ;; if you want fill to be a single value, make it a singular-affine machine
  ;; cont-rightmost is called when the fill machine hits rightmost
  (def-function-class w* (tm fill &optional ➜)
    (:documentation
      "Reads fill, and writes tm, steps both, and repeats until fill passes rightmost;
       it then follows cont-ok.  Should tm hit rightmost first, then cont-rightmost-tm.
       "
      ))

  (defun-typed w* 
    (
      (tm tape-machine)
      fill
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost-tm (be ∅))
        &allow-other-keys
        ) 
      ➜
      (w tm (r fill))
      (⟳(λ(➜again)
          (s fill
            {
              :➜ok (λ()
                    (s tm
                      {
                        :➜ok (λ()
                              (w tm (r fill))
                              [➜again]
                              )
                        :➜rightmost ➜rightmost-tm ; fill's head stepped but tm's didn't
                        }))
              :➜rightmost ➜ok ; we wrote all of fill's instances
              })))))

  (def-function-class s* (tm)
    (:documentation 
      "This is a synonym for ◨. There is no guarantee that intermediate
       cells will be visited."
      ))

  (defun-typed s* ((tm tape-machine)) (◨ tm))

  (def-function-class -s* (tm)
    (:documentation 
      "This is a synonym for ◧. There is no guarantee that intermediate
       cells will be visited."
      ))

  (defun-typed -s*((tm tape-machine))(◧ tm))

  ;; note the fill data will be reversed at the tm insert point
  ;; use as* to fill without reversal
  ;; use eas* to fill forward without moving tm (requires at least nd-tm)
  (def-function-class a* (tm fill &optional ➜)
    (:documentation 
      "Calls #'a repeatedly with successive instances from tm-fill. 
       tm will not be stepped.  tm-fill will be stepped.
       The sequence order as it is found on tm-fill will be reversed on tm.
       "      
      ))

  (defun-typed a*
    (
      (tm tape-machine)
      fill
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (⟳(λ(➜again)
          (a tm (r fill) 
            {
              :➜ok (λ()(s fill {:➜ok ➜again :➜rightmost ➜ok}))
              :➜no-alloc ➜no-alloc
              })
          ))))

  (def-function-class as* (tm fill &optional ➜)
    (:documentation 
      "Calls #'as repeatedly with successive instances from tm-fill.
       Both tm and tm-fill will be stepped.
       "
      ))


  (defun as*-1
    (
      tm 
      fill
      &optional
      (cont-ok (be t))
      (cont-no-alloc #'alloc-fail)
      )
    (⟳(λ(➜again)
        (as tm (r fill) 
          {
            :➜ok (λ()(s fill {:➜ok ➜again :➜rightmost cont-ok}))
            :➜no-alloc cont-no-alloc
            })))
    )

  (defun-typed as*
    (
      (tm0 tape-machine)
      fill
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜no-alloc #'alloc-fail)
        &allow-other-keys
        )
      ➜
      (as*-1 tm0 fill ➜ok ➜no-alloc)
      ))


;;--------------------------------------------------------------------------------
;; repeated by count operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (def-function-class sn (tm n &optional ➜)
    (:documentation 
      "Step n times.  When called, cont-rightmost is passed the current value of n.  For
       example, if the head is on leftmost, and the tape has two cells, and sn is called
       with n set to 3, then the step from rightmost continuation will be called with a
       value of 2.
      "
      ))

  (defun-typed sn
    (
      (tm tape-machine)
      (n integer)
      &optional ➜
      )
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (λ(n)(declare (ignore n)) ∅))
        &allow-other-keys
        )
      ➜
      (⟳(λ(➜again)
          (if
            (> n 0)
            (s tm
              {
                :➜ok (λ()(decf n)[➜again])
                :➜rightmost (λ()[➜rightmost n])
                })
            [➜ok]
            )))
      ))


  (def-function-class asn (tm n &optional fill ➜)
    (:documentation 
      "Similar to calling #'as n times. fill provides initialization
       data. tm and fill are both stepped n times."
      ))

  ;; interesting that the fill iterator is one ahead, pointing at the next
  ;; thing to be written.  Thus this routine does not maintain inclusive bounds.
  (defun-typed asn
    (
      (tm tape-machine)
      (n integer)
      &optional fill  ➜
      )
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost-fill (λ(cnt)(declare (ignore cnt))∅))
        (➜no-alloc (λ(tm n)(declare (ignore tm n))(error 'alloc-fail)))
        )
      ➜
      (⟳(λ(➜again)
          (if
            (> n 0)
            (as tm (r fill)
              {
                :➜ok (λ()
                       (s fill 
                         {
                           :➜ok (λ()(decf n) [➜again])
                           :➜rightmost (λ()[➜rightmost-fill n])
                           }))
              
                :➜no-alloc ➜no-alloc
                })
              [➜ok]
              )))))


;;--------------------------------------------------------------------------------  
;; equivalence
;;   entanglement only works on nd-tape-machines or more special, but we want 
;;   equivalence for all tape machines, hence tm0 and tm1 are not entangle
;;   copied.
;;  
;;
  (def-function-class equiv (tm0 tm1 &optional ➜))
  (defun-typed equiv ((tm0 tape-machine) (tm1 tape-machine) &optional ➜)
    (destructuring-bind
      (&key
        (equiv 
          (λ(tm0 tm1 ct c∅)
            (if
              (equal (r tm0) (r tm1)) 
              [ct]
              [c∅]
              )))
        (➜t (be t))
        (➜∅ (be ∅))
        (➜tm0 (be ∅)) ; ran out of a instances, it needs to be continued to make this equal
        (➜tm1 (be ∅)) ; ran out of b instances
        &allow-other-keys
        )
      ➜
      (⟳(λ(➜again)
          [equiv tm0 tm1
            (λ()
              (s tm0
                {
                  :➜ok (λ() (s tm1
                              {
                                :➜ok ➜again
                                :➜rightmost ➜tm1
                                }))
                  :➜rightmost (λ() (s tm1
                                     {
                                       :➜ok ➜tm0
                                       :➜rightmost ➜t
                                       }))
                  }))
            ➜∅
            ]))))
