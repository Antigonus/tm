#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions are common to all states where they are not directly 
overridden.  

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; status-tm specific
;;

  (defun-typed tm-print ((tm status-tm))
    (princ (type-of tm))
    (princ " ")
    (princ "(")
    (princ (address tm))
    (princ ":")
    (princ (address-rightmost tm))
    (princ ")")
    (princ " ")
    (tm-print (base tm))
    t
    )

  ;; cue head to parked
  (def-function-class cp (tm &optional ➜)) ; handled by subtypes

;;--------------------------------------------------------------------------------
;; quantifiers
;;
  (def-function-class cp∃ (tm pred &optional ➜))
  (def-function-class cp∀ (tm pred &optional ➜))
  (def-function-class cp∃* (tm pred))
  (def-function-class cp∀* (tm function))

;;--------------------------------------------------------------------------------
;; quantified
;;
  (def-function-class cpd* (tm &optional spill ➜)
    (:documentation
      "Deallocates the tape"
      ))


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  ;; specialized versions have been defined for empty and abandoned
  (defun-typed tape-length-is-one ((tm status-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= (address-rightmost tm) 0) [➜t] [➜∅])
      ))
      
  ;; specialized versions have been defined for empty and abandoned
  (defun-typed tape-length-is-two ((tm status-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= (address-rightmost tm) 1) [➜t] [➜∅])
      ))

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;


;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed entangled
    (
      (tm0 status-tm)
      (tm1 status-tm)
      &optional ➜
      )
    (entangled (base tm0) (base tm1) ➜)
    )


