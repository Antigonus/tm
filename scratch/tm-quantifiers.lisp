#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Quantification

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; stepping multiple machines in unison
;;
;; love the null case.  If there are no tms to step, then there does not
;; exist a tm that stepped, so we should return ∅.  On the other hand, if there
;; are no tms to step, then no tm failed to step, so we should return true. 
;; We avoid this end case by requiring that tms be non-void.
;;
  (defun s-together 
    (
      tms ; tms holds tape machines, tms is not moved, but the constituent machines are
      &optional
      (cont-ok #'echo)
      (cont-exists-on-rightmost (be ∅))
      )
    "s-together is passed a non-void list of tape machines.
    Each time s-together is called, all machines in the list are stepped.
    If any of the machines can not be stepped, then none of the machines are stepped.
    cont-exists-end is called with an iterator on the first of the tms that could not
    be stepped.
    "
    (let(
          (tms0 (fork-0 tms))
          (tms1 (fork-0 tms))
          )
      (¬∃ tms0 (λ(tms0)(on-rightmost (r tms0)))
        (λ()
          (⟳(λ(cont-loop cont-return)
              (s (r tms1) #'do-nothing (λ()(error 'tm-impossible-to-get-here)))
              (s tms1 cont-loop cont-return)
              ))
          (funcall cont-ok 's)
          )
        cont-exists-on-rightmost
        )))

;;--------------------------------------------------------------------------------
;; indexed read and write
;;
  (defun fsnr 
    (
      tm
      index
      &optional
      (cont-ok #'echo) ; whenever cont-ok is #'echo, other continuations must throw an error
      (cont-rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "fork tm, step n places, then read."
    (fsnr-0 tm (state tm) index cont-ok cont-rightmost cont-parked)
    )
  (defgeneric fsnr-0 (tm state index cont-ok cont-rightmost cont-parked))
  (defmethod fsnr-0 (tm (state void) index cont-ok cont-rightmost cont-parked)
    (declare (ignore tm state cont-ok))
    (if (= 0 index)
      (funcall cont-parked)
      (funcall cont-rightmost index)
      ))
  (defmethod fsnr-0 (tm (state parked) index cont-ok cont-rightmost cont-parked)
    (if (= 0 index)
      (funcall cont-parked)
      (let(
            (tm1 (fork-0 tm))
            )
        (cue-leftmost tm1) ; this will unpark the head
        (sn tm1 (1- index) 
          (λ()(r tm1 cont-ok #'cant-happen))
          (λ(n)(funcall cont-rightmost n))
          ))))
  (defmethod fsnr-0 (tm (state active) index cont-ok cont-rightmost cont-parked)
    (let(
          (tm1 (fork-0 tm))
          )
      (sn tm1 index
        (λ()(r tm1 cont-ok #'cant-happen))
        (λ(n)(funcall cont-rightmost n))
        )))

  (defun fsnw 
    (
      tm
      object
      index
      &optional
      (cont-ok (be t))
      (cont-rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
      (cont-parked (λ()(error 'parked-head-use)))
      )
    "fork tm, step n places, then write object."
    (fsnw-0 tm (state tm) object index cont-ok cont-rightmost cont-parked)
    )
  (defgeneric fsnw-0 (tm state object index cont-ok cont-rightmost cont-parked))
  (defmethod fsnw-0 (tm (state void) object index cont-ok cont-rightmost cont-parked)
    (declare (ignore tm state object cont-ok))
    (if (= 0 index)
      (funcall cont-parked)
      (funcall cont-rightmost index)
      ))
  (defmethod fsnw-0 (tm (state parked) object index cont-ok cont-rightmost cont-parked)
    (if (= 0 index)
      (funcall cont-parked)
      (let(
            (tm1 (fork-0 tm))
            )
        (cue-leftmost tm1) ; this will unpark the head
        (sn tm1 (1- index) 
          (λ()(w tm1 object cont-ok #'cant-happen))
          (λ(n)(funcall cont-rightmost n))
          ))))
  (defmethod fsnw-0 (tm (state active) object index cont-ok cont-rightmost cont-parked)
    (let(
          (tm1 (fork-0 tm))
          )
      (sn tm1 index
        (λ()(w tm1 object cont-ok #'cant-happen))
        (λ(n)(funcall cont-rightmost n))
        )))
