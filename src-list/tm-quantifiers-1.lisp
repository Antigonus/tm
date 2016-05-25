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
    (⟳ (λ(cont-loop cont-return)
         (w tm (r fill))
         (s-together (mount {tm fill})
           cont-loop
           (λ()
             (if
               (on-rightmost tm)
               (funcall cont-rightmost) ;then we hit the end of tape before finishing
               (funcall cont-return) ;then fill is on rightmost, we are done
               )))))
    (funcall cont-ok)
    )

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

  (defgeneric a* (tm tm-fill &optional cont-ok cont-no-alloc)
    (:documentation 
      "Allocates new cells to tm until running out of fill data."
      ))

  (defgeneric as* (tm tm-fill &optional cont-ok cont-no-alloc)
    (:documentation 
      "Similar to a*, but moves tm's head to the last cell newly allocated."
      ))

  (defun as*-0 
    (
      tm 
      fill
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (⟳ (λ(cont-loop cont-return)
         (as tm (r fill) 
           #'do-nothing
           (λ()(return-from as*-0 (funcall cont-no-alloc)))
           )
         (s fill cont-loop cont-return)
         ))
    (funcall cont-ok)
    )

  (defmethod a*
    (
      (tm0 tape-machine) 
      fill
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (let(
          (tm1 (dup-0 tm0))
          )
      (as*-0 tm1 fill cont-ok cont-no-alloc)
      ))

  (defmethod as*
    (
      (tm0 tape-machine) 
      fill
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (as*-0 tm0 fill cont-ok cont-no-alloc)
    )

  (defgeneric d* (tm &optional spill 
                  cont-ok 
                  cont-not-supported
                  cont-collision
                  cont-no-alloc)
    (:documentation 
      "Deallocates all cells right of the head up to and including rightmost.
       If spill is not ∅, then the deallocated right side is moved to it.  Preferably the
       cells are moved, but often the objects are reallocated to spill using #'as.
      "
      ))

  (defmethod d*
    (
      (tm tape-machine)
      &optional 
      spill
      (cont-ok (be t))
      (cont-not-supported (λ()(error 'not-supported)))
      (cont-collision (λ()(error 'dealloc-entangled)))
      (cont-no-alloc (λ()(error 'alloc-fail)))
      )
    (labels(
             (do-work()
               (d tm spill 
                 (λ(object) 
                   (declare (ignore object))
                   (funcall #'do-work)
                   )
                 cont-ok ; this is at rightmost continuation for #'d
                 cont-not-supported
                 cont-collision
                 cont-no-alloc
                 ))
             )
      (do-work)
      ))

;;--------------------------------------------------------------------------------
;; repeated by count operations
;;   more specific versions, if they exist, are surely more efficient
;;
  (defgeneric sn (tm n &optional cont-ok cont-rightmost)
    (:documentation 
      "Step n times.  When called, cont-rightmost is passed the current value of n.  For
       example, if the head is on leftmost, and the tape has two cells, and sn is called
       with n set to 3, then the step from rightmost continuation will be called with a
       value of 2.
      "
      ))

  (defmethod sn
    (
      (tm tape-machine)
      (n integer)
      &optional 
      (cont-ok (be t))
      (cont-rightmost (λ(n)(declare (ignore n)) ∅))
      )
    (labels(
             (count-test()
               (when (≤ n 0) (return-from sn (funcall cont-ok)))
               )
             (work()
               (decf n)
               (count-test)
               (take-step) ; step from rightmost test is built into #'s
               )
             (take-step()
               (s 
                 tm 
                 #'work
                 (λ()(return-from sn (funcall cont-rightmost n)))
                 ))
             )
      (count-test)
      (take-step)
      ))

  (defgeneric an (tm tm-fill count &optional cont-ok cont-rightmost)
    (:documentation 
      "Similar to calling #'a n times on a dup of tm."
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


;;--------------------------------------------------------------------------------
;; indexed read and write
;;
  (defun csnr 
    (
      tm
      index
      &optional
      (cont-ok #'echo) ; whenever cont-ok is #'echo, other continuations must throw an error
      (cont-rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "dup tm, step n places, then read."
    (csnr-0 tm (state tm) index cont-ok cont-rightmost cont-parked)
    )
  (defgeneric csnr-0 (tm (state tm) index cont-ok cont-rightost cont-parked))
  (defmethod csnr-0 (tm (state void) index cont-ok cont-rightost cont-parked)
    (declare (ignore tm state index cont-ok))
    (if (= 0 index)
      (funcall cont-parked)
      (funcall cont-rightmost index)
      ))
  (defmethod csnr-0 (tm (state parked) index cont-ok cont-rightost cont-parked)
    (if (= 0 index)
      (funcall cont-parked)
      (let(
            (tm1 (dup-0 tm))
            )
        (cue-leftmost tm1) ; this will unpark the head
        (sn tm1 (1- index) 
          (λ()(r tm cont-ok #cant-happen))
          (λ(n)(funcall cont-rightmost n))
          ))))
  (defmethod csnr-0 (tm (state active) index cont-ok cont-rightost cont-parked)
    (let(
          (tm1 (dup-0 tm))
          )
      (sn tm1 index
        (λ()(r tm cont-ok #cant-happen))
        (λ(n)(funcall cont-rightmost n))
        )))

  (defun csnw 
    (
      tm
      object
      index
      &optional
      (cont-ok (be t))
      (cont-rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "dup tm, step n places, then write object."
    (csnw-0 tm (state tm) object index cont-ok cont-rightmost cont-parked)
    )
  (defgeneric csnw-0 (tm (state tm) index cont-ok cont-rightost cont-parked))
  (defmethod csnw-0 (tm (state void) index cont-ok cont-rightost cont-parked)
    (declare (ignore tm state index cont-ok))
    (if (= 0 index)
      (funcall cont-parked)
      (funcall cont-rightmost index)
      ))
  (defmethod csnw-0 (tm (state parked) index cont-ok cont-rightost cont-parked)
    (if (= 0 index)
      (funcall cont-parked)
      (let(
            (tm1 (dup-0 tm))
            )
        (cue-leftmost tm1) ; this will unpark the head
        (sn tm1 (1- index) 
          (λ()(w tm1 object cont-ok #'cant-happen))
          (λ(n)(funcall cont-rightmost n))
          ))))
  (defmethod csnw-0 (tm (state active) index cont-ok cont-rightost cont-parked)
    (let(
          (tm1 (dup-0 tm))
          )
      (sn tm1 index
        (λ()(w tm1 object cont-ok #'cant-happen))
        (λ(n)(funcall cont-rightmost n))
        )))
