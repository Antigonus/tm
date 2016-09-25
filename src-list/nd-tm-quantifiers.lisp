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
;; all step, or none step.
;;
;; love the null case.  If there are no tms to step, then there does not
;; exist a tm that stepped, so we should return ∅.  On the other hand, if there
;; are no tms to step, then no tm failed to step, so we should return true. 
;; ... at the level of implementation here in src-list, machines are not
;; stateful, so we can not pass in a void machine.
;;
  (defun s-together 
    (
      tms ; objects are tms to be stepped
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    "s-together is passed a tm that has other tape machines as objects.
    Each time s-together is called, the constituent machines are stepped.  If any of the
    machines can not be stepped, then none of the machines are stepped.  When
    cont-rightmost is taken, tms will be left pointing at the first machine that could not
    be stepped.
    "
    (cue-leftmost tms)
    (¬∃ tms (λ(tms)(on-rightmost (r tms)))
      (λ() 
        (cue-leftmost tms)
        (∨ (∀ tms (λ(tms)(s (r tms)))) (funcall #'cant-happen))
        (funcall cont-ok)
        )
      cont-rightmost
      ))


;;--------------------------------------------------------------------------------
;; indexed read and write
;;
  (defgeneric esnr 
    (
      tm
      index
      &optional
      cont-ok
      cont-rightmost
      )
    (:documentation
      " This is an indexed read operation.  It makes an entangled copy of 'tm', 
        steps it 'index' times, then reads it.
      "
      ))

  (defmethod esnr 
    (
      tm
      index
      &optional
      (cont-ok #'echo)
      (cont-rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
      )
    (with-mk-entangled tm
      (λ(tm1)
        (sn tm1 index
          (λ()(funcall cont-ok (r tm1)))
          (λ(n)(funcall cont-rightmost n))
          ))))

  (defgeneric esnw 
    (
      tm
      index
      object
      &optional
      cont-ok
      cont-rightmost
      )
    (:documentation
      " This is an indexed write operation.  It makes an entangled copy of 'tm', 
        steps it 'index' places, then writes the 'object'.
      "
      ))

  (defmethod esnw 
    (
      tm
      index
      object
      &optional
      (cont-ok (be t))
      (cont-rightmost (λ(index)(declare (ignore index))(error 'step-from-rightmost)))
      )
    (with-mk-entangled tm
      (λ(tm1)
        (sn tm1 index
          (λ()(w tm1 object cont-ok #'cant-happen))
          (λ(n)(funcall cont-rightmost n))
          ))))
