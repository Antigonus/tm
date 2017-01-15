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
  (defun ⟳ (work &rest ⋯)
    "⟳ (pronounced \"do\") accepts a 'work' function and arguments. work may be a nameless
     lambda. ⟳ prepends a value to the argument list and calls work.  When the prepended
     value is called with funcall, work is called again with the same arguments.
     "
    (labels(
             (again() (apply #'funcall {work #'again (o ⋯)}))
             )
      (again)
      ))

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
    "Tests each instance in tm in succession starting from the current location of the head.
     Exits with cont-true upon the test passing.  Otherwise returns cont-false when stepping
     right from rightmost.
    "
    (⟳(λ(again)[pred tm cont-true (λ()(s tm again cont-false))]))
    )

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
    (∃ tm pred 
      (λ()(⟳ (λ(again)(s tm again cont-true)))) ; exhausts the tape
      cont-false
      )
    )

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

