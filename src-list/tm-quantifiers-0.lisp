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
     single step, whatever such a step may be.  The work function accepts two
     continuations.  Typically these are called 'cont-loop', and 'cont-return'.  When the
     work function continues with cont-loop, it is immediately called again.  When it
     continues with cont-return, ⟳ returns.
     "
    (labels(
             (do-work ()
               (funcall work
                 #'do-work
                 (λ() (return-from ⟳ t))
                 ))
             )
      (do-work)
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
;; careful - the quantifiers start where the head is located, they do not cue-leftmost first
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
    (⟳ (λ(cont-loop cont-return)
         (when 
           (funcall pred tm) 
           (return-from ∃ (funcall cont-true))
           )
         (s tm cont-loop cont-return)
         ))
    (funcall cont-false)
    )

  ;; same as step-while
  ;; not all objects match pred, here we will show you the first one that doesn't
  ;; there exists an object for which pred is false
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
        (⟳ (λ(cont-loop cont-return)(∃*-op)(s tm cont-loop cont-return)))
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


;;--------------------------------------------------------------------------------
;; stepping multiple machines in unison
;;
;; love the null case.  If there are no tms to step, then there does not
;; exist a tm that stepped, so we should return ∅.  On the other hand, if there
;; are no tms to step, then no tm failed to step, so we should return true. 
;; I aemptyed this issue by passing in a tm to the sequence of tms. Such a sequence
;; must have one member to exist.
;;
  (defun s-together 
    (
      tms ; tms hold tape machines, tms is not moved, but the constituent machines are
      &optional
      (cont-ok #'echo)
      (cont-exists-on-rightmost (be ∅))
      )
    "s-together is passed a list of tape machines.
    Each time s-together is called, all machines in the list are stepped.
    If any of the machines can not be stepped, then none of the machines are stepped.
    cont-exists-end is called with an iterator on the first of the tms that could not
    be stepped.
    "
    (let(
          (tms0 (dup-0 tms))
          (tms1 (dup-0 tms))
          )
      (if
        (¬∃ tms0 #'on-rightmost)
        (progn
          (⟳ (λ(cont-loop cont-return)
               (s (r tms1) #'do-nothing (λ()(error 'tm-impossible-to-get-here)))
               (s tms1 cont-loop cont-return)
               ))
          (funcall cont-ok 's)
          )
        (funcall cont-exists-on-rightmost)
        )))


