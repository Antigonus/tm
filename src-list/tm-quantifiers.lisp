#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Quantification

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; looping, see also s-together⟳  --> need to replace these with quantifiers
;;
  (defun ⟳ (work)
    "⟳ (pronounced \"do\") accepts a work function.  The work function is passed a single
     continuation argument.  If the continuation is called, the work funciton is repeated.
     The return value is that returned by work when it doesn't repeat.
     "
    (labels(
             (do-work () [work #'do-work])
             )
      (do-work)
      ))

  (defun ⟳-loop (work)
    "⟳ (pronounced \"do\") accepts a work function.  This work function is to take a
     single step, whatever such a step may be.  The work function accepts a loop
     continuation function, typically called cont-loop.  When the work function continues
     with cont-loop, it is immediately called again.  When the work function returns
     so does ⟳-loop.
     "
    (labels(
             (do-work () [work (λ()(return-from do-work (funcall #'do-work)))])
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
      [work (λ(&rest vs)(return-from ⟳-return (values-list vs)))]
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
                 [work]
                 [step tm #'do-work (λ()(return-from ⟳-work-step))]
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
;; trivial predicates 
;;
  (defun always-false (tm &optional (cont-true (be t)) (cont-false (be ∅)))
    (declare (ignore tm cont-true))
    [cont-false]
    )

  (defun always-true (tm &optional (cont-true (be t)) (cont-false (be ∅)))
    (declare (ignore tm cont-false))
    [cont-true]
    )


;;--------------------------------------------------------------------------------
;; quantification
;;
;; careful:
;;
;; The quantifiers start where the head is located, they do not cue-leftmost first.  I do
;; this so that prefix values may be processed before calling a quantifier.
;;
;; I pass the tape machine to the predicate rather than the instance in the cell the head
;; is on.  I do this so that predicates may use the head as a general marker on the tape,
;; for example, as the origin for a sliding window.
;;
;; pred is a function that accepts a machine and two continuations, cont-true, and cont-false.
;;
  (defun ∃ 
    (
      tm
      pred
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    "When returning true, tm head is on the first cell that has an instance where pred is true.
     When returning false, tm head is on rightmost, and there was no cell where pred was true.
    "
    (labels(
             (test() [pred tm cont-true #'step-retest])
             (step-retest() (s tm #'test cont-false))
             )
      (test)
      ))

  ;; There exists an instance for which pred is false.
  ;; Same as step-while
  ;; Not all instances match pred, here we will show you the first one that doesn't.
  (defun ¬∀
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true, all instances do not match pred, and tm is on the first mismatch."
    (∃ tm (λ(tm ct c∅)[pred tm c∅ ct]) cont-true cont-false)
    )

  ;; there does not exist an instance for which pred is true
  ;; i.e. pred is false for all instances
  (defun ¬∃ 
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true, there does not exist an instance where pred holds, and tm is at rightmost.
    When false, tm is on an cell with an instance where pred holds."
    (∃ tm pred cont-false cont-true)
    )

  ;; there does not exist an instance for which pred is false
  ;; pred is true for all instances
  (defun ∀
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true all instances match pred.  When false tm will be on the first mismatch."
    (∃ tm (λ(tm ct c∅)[pred tm c∅ ct]) cont-false cont-true)
    )

  (defun ∃*
    (
      tm
      pred
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    "When returning true, tm head is on the first cell that has an instance where pred is true.
     When returning false, tm head is on rightmost, and there was no cell where pred was true.
    "
    (labels(
             (test ()
               [pred tm #'exhaust-tape #'step-retest]
               )
             (step-retest ()
               (s tm #'test cont-false)
               )
             (exhaust-tape ()
               (s tm #'exhaust-tape cont-true)
               )
             )
      (test)
      ))

  (defun ¬∀* 
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true, all instances do not match pred, and tm is on the first mismatch."
    (∃* tm (λ(tm ct c∅)[pred tm c∅ ct]) cont-true cont-false)
    )

  (defun ¬∃*
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true, there does not exist an instance where pred holds, and tm is at rightmost.
    When false, tm is on an cell with an instance where pred holds."
    (∃* tm pred cont-false cont-true)
    )

  (defun ∀*
    (
      tm
      pred 
      &optional 
      (cont-true (be t)) 
      (cont-false (be ∅))
      )
    "When true all instances match pred.  When false tm will be on the first mismatch."
    (∃* tm (λ(tm ct c∅)[pred tm c∅ ct]) cont-false cont-true)
    )

