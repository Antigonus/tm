#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Quantification

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; repeated until end of tape operations
;;   more specific versions, if they exist, are surely more efficient
;;
  ;; if you want fill to be a single value, make it a singular-affine machine
  ;; cont-rightmost is called when the fill machine hits rightmost
  (defgeneric w* (tm fill &optional cont-ok cont-rightmost))

  (defmethod w* 
    (
      (tm tape-machine)
      (fill tape-machine)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (⟳-when
      (λ(cont-loop)
        (w tm (r fill))
        (s-together (mount {tm fill})
          cont-loop
          (λ()
            (if
              (on-rightmost tm)
              (funcall cont-rightmost) ;then we hit the end of tape before finishing
              (funcall cont-ok) ;then fill is on rightmost, we are done
              ))))))

  (defgeneric s* (tm)
    (:documentation 
      "This is a synonym for cue-to-rightmost. There is no guarantee that intermediate
       cells will be visited."
      ))

  (defmethod s* ((tm tape-machine)) (cue-rightmost tm))

  (defgeneric -s* (tm)
    (:documentation 
      "This is a synonym for cue-to-leftmost. There is no guarantee that intermediate
       cells will be visited."
      ))

  (defmethod -s*((tm tape-machine))(cue-leftmost tm))

  (defgeneric a* (tm tm-fill &optional cont-ok cont-not-supported cont-no-alloc)
    (:documentation 
      "Calls #'a repeatedly with successive objects from tm-fill. 
       tm will not be stepped.  tm-fill will be stepped.
       The sequence order as it is found on tm-fill will be reversed on tm.
       "      
      ))

  (defgeneric as* (tm tm-fill &optional cont-ok cont-not-supported cont-no-alloc)
    (:documentation 
      "Calls #'as repeatedly with successive objects from tm-fill.
       Both tm and tm-fill will be stepped.
       "
      ))

  (defgeneric fas* (tm tm-fill &optional cont-ok cont-not-supported cont-no-alloc)
    (:documentation 
      "Forks tm, calls #'as on the fork repeatedly with successive objects from tm-fill.
       tm will not be stepped.
       tm-fill will be stepped.
       "
      ))

  ;; cont-no-alloc is not transactional here ... need to fix the other a* versions too
  ;; do we want it to be transactional?  But if it did a spot fix it would have to be
  ;; possible to restart the a* where we left off ..
  (defmethod a*
    (
      (tm tape-machine) 
      fill
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (supports-alloc tm
      (λ()
        (⟳-when
          (λ(cont-loop)
            (a tm (r fill) 
              (λ()(s fill cont-loop cont-ok))
              #'cant-happen
              cont-no-alloc
              ))))
      cont-not-supported
      ))

  (defun as*-0 
    (
      tm 
      fill
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (supports-alloc tm
      (λ()
        (⟳-when
          (λ(cont-loop)
            (as tm (r fill) 
              (λ()(s fill cont-loop cont-ok))
              #'cant-happen
              cont-no-alloc
              ))))
      cont-not-supported
      ))

  (defmethod fas*
    (
      (tm0 tape-machine) 
      fill
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (let(
          (tm1 (fork-0 tm0))
          )
      (as*-0 tm1 fill cont-ok cont-not-supported cont-no-alloc)
      ))

  (defmethod as*
    (
      (tm0 tape-machine) 
      fill
      &optional
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (as*-0 tm0 fill cont-ok cont-not-supported cont-no-alloc)
    )

   ;; dealloc single cell functions return on the object in the cell as that is a natural
   ;; flow for single cell operations.  However, for multiple cell operations we use the
   ;; spill mechanism.
   (defun d* (tm &optional spill 
               (cont-ok (be t))
               (cont-collision (λ()(error 'dealloc-entangled)))
               (cont-not-supported (λ()(error 'not-supported)))
               (cont-spill-not-supported (λ()(error 'spill-not-supported)))
               (cont-spill-no-alloc (λ()(error 'alloc-fail)))
               )
     "Deallocates all cells right of the head up to and including rightmost.
       If spill is not ∅, then the deallocated right side is moved to it.  Preferably
       the cells are moved, but often the objects are reallocated to spill using #'as.
      "
     (d*-1 tm (state tm) spill cont-ok cont-collision cont-not-supported cont-spill-not-supported cont-spill-no-alloc)
     )

  (defgeneric d*-1 
    (
      tm
      state
      spill 
      cont-ok
      cont-collision 
      cont-not-supported 
      cont-spill-not-supported 
      cont-spill-no-alloc
      ))
  (defmethod d*-1
    (
      tm
      (state void)
      spill
      cont-ok
      cont-collision
      cont-not-supported
      cont-spill-not-supported
      cont-spill-no-alloc
      )
    (declare (ignore tm spill cont-collision cont-not-supported cont-spill-not-supported cont-spill-no-alloc))
    (funcall cont-ok)
    )
  (defmethod d*-1
    (
      tm
      (state parked)
      spill
      cont-ok
      cont-collision
      cont-not-supported
      cont-spill-not-supported
      cont-spill-no-alloc
      )
    (declare (ignore cont-not-supported))
    (∀-parked tm
      (λ()
        (if spill
          (progn
            (cue-leftmost tm) ; head was parked
            (a* spill tm
              (λ()
                (void tm)
                (funcall cont-ok)
                )
              cont-spill-not-supported
              cont-spill-no-alloc
              ))
          (progn
            (void tm)
            (funcall cont-ok)
            )))
      cont-collision
      ))

  ;; called from d*-1
  (defun d*-0 (tm)
    (⟳(λ(cont-loop cont-return)
        (on-rightmost tm
          cont-return
          (λ()(d-0 tm cont-loop #'cant-happen))
          ))))

  (defmethod d*-1
    (
      tm
      (state active)
      spill
      cont-ok
      cont-collision
      cont-not-supported
      cont-spill-not-supported
      cont-spill-no-alloc
      )
    (on-rightmost tm
      cont-ok
      (λ()
        (supports-dealloc tm
          (λ()
            (∃-collision-right tm
              cont-collision
              (λ()
                (if spill
                  (let(
                        (tm0 (fork-0 tm))
                        )
                    (s tm0 #'do-nothing #'cant-happen) ; positions head of fill tm0 for a*
                    (a* spill tm0
                      (λ()(d*-0 tm)) ; note use of tm rather than tm0
                      cont-spill-not-supported
                      cont-spill-no-alloc
                      ))
                  (d*-0 tm)
                  )))))
        cont-not-supported
        )))

;;--------------------------------------------------------------------------------
;; repeated by count operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (defgeneric an (tm tm-fill count &optional cont-ok cont-rightmost)
    (:documentation 
      "Similar to calling #'a n times on a fork of tm."
      ))

  (defgeneric asn (tm tm-fill n &optional cont-ok cont-rightmost)
    (:documentation 
      "Similar to calling #'as n times. fill is tm that provides initialization
       data. tm and fill are both stepped n times."
      ))

  (defgeneric dn (tm count &optional spill cont-ok cont-rightmost)
    (:documentation
      "Given a tape machine and a natural number.
      Like repeating d count times, but specialized versions might be more efficient.
      "
      ))

  (defmethod dn
    (
      (tm tape-machine)
      (n integer)
      &optional 
      spill
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (labels(
             (do-work()
               (when (≤ n 0) (return-from dn (funcall cont-ok)))
               (d tm spill 
                 #'do-work
                 (λ()(return-from dn (funcall cont-rightmost n)))
                 )
               (decf n)
               )
             )
      (do-work)
      ))


