#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions are common to all states where they are not directly 
overridden.  

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; some status specific helpers
;;
  (defmacro def-abandoned-1 (f &rest args)
    `(defun-typed ,f ((tm abandoned) ,@args &optional ➜)
       (declare (ignore ,@args ➜))
       (operation-on-abandoned)
       )
    )

  (defmacro def-empty-1 (f &rest args)
    `(defun-typed ,f ((tm empty) ,@args &optional ➜)
       (declare (ignore ,@args))
       (destructuring-bind
         (
           &key
           (➜empty #'use-of-empty)
           &allow-other-keys
           )
         ➜
         [➜empty]
         ))
    )

  (defmacro def-parked-1 (f &rest args)
    `(defun-typed ,f ((tm parked) ,@args &optional ➜)
       (declare (ignore ,@args))
       (destructuring-bind
         (
           &key
           (➜parked #'access-through-parked-head)
           &allow-other-keys
           )
         ➜
         [➜parked]
         ))
    )

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
  (def-function-class hp (tm &optional ➜)) ; handled by subtypes

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


