#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Quantification

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; looping, see also s-together⟳
;;
  (defun ⟳ (work)
    "⟳ (pronounced \"do\") accepts a work function.  This work function is to take a
     single step, whatever such a step may be.  The work function accepts two arguments,
     both functions, one typically called cont-loop, the other typically called
     cont-return.  When the work function continues with cont-loop, it is immediately
     called again.  When the work function returns, or when cont-return is called,
     both the loop function and ⟳ return.
     "
    (labels(
             (do-work () 
               (funcall 
                 work
                 (λ()(return-from do-work (funcall #'do-work)))
                 (λ(&rest vs)(return-from ⟳ (values-list vs)))
                 ))
             )
      (do-work)
      ))

  (defun ⟳-loop (work)
    "⟳ (pronounced \"do\") accepts a work function.  This work function is to take a
     single step, whatever such a step may be.  The work function accepts a loop
     continuation function, typically called cont-loop.  When the work function continues
     with cont-loop, it is immediately called again.  When the work function returns
     so does ⟳.
     "
    (labels(
             (do-work () (funcall work (λ()(return-from do-work (funcall #'do-work)))))
             )
      (do-work)
      ))

  (defun ⟳-return (work)
    "⟳-return accepts a work function.  This work function is to take a
     single step, whatever such a step may be.  The work function accepts a loop
     exit function, typically called cont-return.  When the work function continues
     with cont-return, the work function and the loop return.
     "
    (loop
      (funcall work (λ(&rest vs)(return-from ⟳-return (values-list vs))))
      ))

  ;; This version of ⟳ facilitates the programmer in making the stepping function explicit
  ;; as an argument. The machine to be stepped, and the function to use to step it, are
  ;; explicitly provided as arguments.
  ;; 
  (defun ⟳-work-step
    (
      tm 
      &optional 
      (work #'do-nothing) 
      (step #'s)
      )
    "⟳-work-step (pronounced \"do work step\") accepts a tape machine, a function to do
     work, and a step function.  The step function must accept as arguments the tape
     machine, and two continuations.  Typically the continuations are called 'cont-loop' and
     'cont-return'.  For example #'s can be used for stepping.  First the
     work function is called, then the step function is called. If the step function
     continues with cont-loop, ⟳-step repeats.  If it continues with continue-return
     ⟳-step returns.
     "
      (labels(
               (do-work ()
                 (funcall work)
                 (funcall step tm #'do-work (λ()(return-from ⟳-work-step)))
                 )
               )
        (do-work)
        ))

;;--------------------------------------------------------------------------------
;; quaternion relationship among quantifiers
;;   q00 is existential quantification
;;
  (defmacro q01 (q00 tm pred) 
    `(funcall ,q00 ,tm (λ(i)(not (funcall ,pred i))))
    )

  (defmacro q10 (q00 tm pred) 
    `(not (funcall ,q00 ,tm ,pred))
    )

  (defmacro q11 (q00 tm pred) 
    `(not (funcall ,q00 ,tm (λ(i)(not (funcall ,pred i)))))
    )

;;--------------------------------------------------------------------------------
;; quantification
;;
;; careful:
;;
;; The quantifiers start where the head is located, they do not cue-leftmost first.  I do
;; this so that prefix values may be processed before calling a quantifier.
;;
;; I pass the tape machine to the predicate rather than the object in the cell the head
;; is on.  I do this so that predicates may use the head as a general marker on the tape,
;; for example, as the origin for a sliding window.
;;
  (defun ∃ 
    (
      tm
      pred
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    "When returning true, tm head is on the first cell that has an object where pred is true.
    When returning false, tm head is on rightmost, and there was no cell where pred was true."
    (⟳-loop
      (λ(cont-loop)
        (when 
          (funcall pred tm) 
          (return-from ∃ (funcall cont-true))
          )
        (s tm cont-loop (λ()(return-from ∃ (funcall cont-false))))
        )))

  ;; There exists an object for which pred is false.
  ;; Same as step-while
  ;; Not all objects match pred, here we will show you the first one that doesn't.
  (defun ¬∀
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true, all objects do not match pred, and tm is on the first mismatch."
    (if 
      (q01 #'∃ tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

  ;; there does not exist an object for which pred is true
  ;; i.e. pred is false for all objects
  (defun ¬∃ 
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true, there does not exist an object where pred holds, and tm is at rightmost.
    When false, tm is on an cell with an object where pred holds."
    (if 
      (q10 #'∃ tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

  ;; pred is true for all objects
  (defun ∀
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true all objects match pred.  When false tm will be on the first mismatch."
    (if
      (q11 #'∃ tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defun ∃*
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "like ∃, but all objects will be visited"
    (let(
          (result ∅)
          )
      (labels(
               (∃*-op () (when (funcall pred tm) (setq result t)))
               )
        (⟳(λ(cont-loop cont-return)(∃*-op)(s tm cont-loop cont-return)))
        (if 
          result
          (funcall cont-true)
          (funcall cont-false)
          ))))

  (defun ¬∀* 
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    (if
      (q01 #'∃* tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defun ¬∃*
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    (if
      (q10 #'∃* tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defun ∀*
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    (if
      (q11 #'∃* tm pred)
      (funcall cont-true)
      (funcall cont-false)
      ))

