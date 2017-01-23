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
             (again() (apply work (cons #'again ⋯)))
             )
      (again)
      ))

;;--------------------------------------------------------------------------------
;; trivial predicates 
;;
  (defun always-false (tm &optional ➜)
    (declare (ignore tm))
    (destructuring-bind 
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  (defun always-true (tm &optional ➜)
    (declare (ignore tm))
    (destructuring-bind 
      (&key
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      [➜t]
      ))

;;--------------------------------------------------------------------------------
;; quantification
;;
;; careful:
;;
;; The quantifiers start where the head is located, they do not cue-leftmost first.  I do
;; this so that prefix values may be processed before calling a quantifier.
;;
;; I pass to the predicate the entire tape machine, rather than just the instance in the
;; cell the head is on.  I do this so that predicates may use the head as a general marker
;; on the tape, for example, as the origin for a sliding window.
;;
;; pred is a function that accepts a machine and two continuations, ➜t, and ➜∅.
;;
  (defun ∃ (tm pred &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        )
      ➜
    "Tests each instance in tm in succession starting from the current location of the head.
     Exits via ➜t upon the test passing.  Otherwise steps and repeats. Exits via
     ➜∅ when stepping right from rightmost.  The head is left on the cell that holds the
     instance that passed.
    "
    (⟳(λ(again)[pred tm {:➜t ➜t :➜∅ (λ()(s tm {:➜ok again :➜rightmost ➜∅}))}]))
    )

  ;; there does not exist an instance for which pred is false
  ;; pred is true for all instances
  (defun ∀ (tm pred &optional ➜)
    "➜t when all instances on the tape pass the test, otherwise ➜∅, head left on cell with first failed test."
    (destructuring-bind
      (&key
        ((:➜t cont-true) (be t)) 
        ((:➜∅ cont-false) (be ∅))
        )
      ➜
      (∃ tm
        (λ(tm ➜)
          (destructuring-bind (&key ➜t ➜∅) ➜
            [pred tm {:➜t ➜∅ :➜∅ ➜t}]
            ))
        {:➜t cont-false :➜∅ cont-true}
        )))

  (defun ∃* (tm pred &optional ➜)
    "∃ and always exits with the head on the rightmost cell."
    (destructuring-bind
      (&key
        ((:➜t cont-true) (be t))
        ((:➜∅ cont-false) (be ∅))
        )
      (∃ tm pred
        {
          :➜t (λ()(⟳ (λ(again)(s tm {:➜ok again :➜rightmost cont-true})))) ; exhausts the tape
          :➜∅ cont-false
          })))

  (defun ∀* (tm pred &optional ➜)
    "∀ and always exists with the head on the rightmost cell>"
    (destructuring-bind
      (&key
        ((:➜t cont-true) (be t)) 
        ((:➜∅ cont-false) (be ∅))
        )
      ➜
      (∃* tm
        (λ(tm ➜)
          (destructuring-bind (&key ➜t ➜∅) ➜
            [pred tm {:➜t ➜∅ :➜∅ ➜t}]
            ))
        {:➜t cont-false :➜∅ cont-true}
        )))


