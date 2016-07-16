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
    (with-mk-entangled tms
      (λ(tms0)
        (with-mk-entangled tms
          (λ(tms1)
            (¬∃ tms0 (λ(tms0)(on-rightmost (r tms0)))
              (λ()
                (⟳(λ(cont-loop cont-return)
                    (s (r tms1) #'do-nothing (λ()(error 'tm-impossible-to-get-here)))
                    (s tms1 cont-loop cont-return)
                    ))
                (funcall cont-ok 's)
                )
              cont-exists-on-rightmost
              ))))))

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
        steps it 'index' places, then reads it.
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
          (λ()(r tm1 cont-ok #'cant-happen))
          (λ(n)(funcall cont-rightmost n))
          ))))

  (defgeneric esnw 
    (
      tm
      object
      index
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
      object
      index
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
