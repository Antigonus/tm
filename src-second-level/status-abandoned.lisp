#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

It is a program bug to use an abandoned machine and we throw an exception. We currently
don't provide a continuation for it.

|#

(in-package #:tm)

(defmacro def-abandoned-1 (f &rest args)
  `(defun-typed ,f ((tm status-abandoned) ,@args &optional ➜)
     (declare (ignore ,@args ➜))
     (operation-on-abandoned)
     )
  )

;;--------------------------------------------------------------------------------
;; status-tm definitions
;;
  (def-abandoned-1 park)

;;--------------------------------------------------------------------------------
;; quantifiers
;;

  (defun-typed ∃ ((tm status-abandoned) pred &optional (➜t (be t)) (➜∅ (be ∅)))
    (declare (ignore tm pred ➜t ➜∅))
    (operation-on-abandoned)
    )

  (defun-typed c◧∃ ((tm status-abandoned) pred &optional (➜t (be t)) (➜∅ (be ∅)))
    (declare (ignore tm pred ➜t ➜∅))
    (operation-on-abandoned)
    )

  (defun-typed ∀ ((tm status-abandoned) pred &optional (➜t (be t)) (➜∅ (be ∅)))
    (declare (ignore tm pred ➜t ➜∅))
    (operation-on-abandoned)
    )

  (defun-typed c◧∀ ((tm status-abandoned) pred &optional (➜t (be t)) (➜∅ (be ∅)))
    (declare (ignore tm pred ➜t ➜∅))
    (operation-on-abandoned)
    )

  (defun-typed ∃* ((tm status-abandoned) pred)
    (declare (ignore tm pred))
    (operation-on-abandoned)
    )
  (defun-typed c◧∃* ((tm status-abandoned) pred)
    (declare (ignore tm pred))
    (operation-on-abandoned)
    )

  (defun-typed ∀* ((tm status-abandoned) function)
    (declare (ignore tm function))
    (operation-on-abandoned)
    )
  (defun-typed c◧∀* ((tm status-abandoned) function)
    (declare (ignore tm function))
    (operation-on-abandoned)
    )


;;--------------------------------------------------------------------------------
;; tm-decl-only
;;
  (def-abandoned-1 r)
  (def-abandoned-1 esr)
  (def-abandoned-1 w instance)
  (def-abandoned-1 esw instance)

  (def-abandoned-1 ec◧r)
  (def-abandoned-1 ec◧w instance)

  (def-abandoned-1 c◧)
  (def-abandoned-1 s)
  (def-abandoned-1 -s)
  (def-abandoned-1 a instance)
  (def-abandoned-1 on-leftmost)
  (def-abandoned-1 on-rightmost)

  (def-abandoned-1 tape-length-is-one)
  (def-abandoned-1 tape-length-is-two)

;;--------------------------------------------------------------------------------
;; tm-generic
;;
  (def-abandoned-1 c◨)
  (def-abandoned-1 as instance) 
  (def-abandoned-1 a&h◨ instance)
  (def-abandoned-1 as&h◨ instance)

;;--------------------------------------------------------------------------------
;; solo-tm-decl-only
;;
  (def-abandoned-1 a◧ instance)
  (defun-typed d
    (
      (tm status-abandoned)
      &optional spill ➜
      )
    (declare (ignore tm spill ➜))
    (operation-on-abandoned)
    )
  (defun-typed d◧
    (
      (tm status-abandoned)
      &optional spill ➜
      )
    (declare (ignore tm spill ➜))
    ;; (prins (print "d◧ status-abandoned"))
    (operation-on-abandoned)
    )
  (defun-typed d.
     (
      (tm status-abandoned)
      &optional spill ➜
      )
    (declare (ignore tm spill ➜))
    (operation-on-abandoned)
    )


;;--------------------------------------------------------------------------------
;; nd-tm-decl-only
;;
  (defun-typed entangled
    (
      (tm0 status-abandoned)
      (tm1 tape-machine)
      &optional ➜
      )
    (declare (ignore tm0 tm1 ➜))
    (operation-on-abandoned)
    )

  (defun-typed entangled
    (
      (tm0 tape-machine)
      (tm1 status-abandoned)
      &optional ➜
      )
    (declare (ignore tm0 tm1 ➜))
    (operation-on-abandoned)
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 status-abandoned)
      (tm1 status-abandoned)
      &optional ➜
      )
    (declare (ignore tm0 tm1 ➜))
    (operation-on-abandoned)
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 status-abandoned)
      (tm1 tape-machine)
      &optional ➜
      )
    (declare (ignore tm0 tm1 ➜))
    (operation-on-abandoned)
    )

  (defun-typed heads-on-same-cell
    (
      (tm0 tape-machine)
      (tm1 status-abandoned)
      &optional ➜
      )
    (declare (ignore tm0 tm1 ➜))
    (operation-on-abandoned)
    )

;;--------------------------------------------------------------------------------
;; nd-tm-generic
;;
  (defun-typed s≠ 
    (
      (tm0 status-abandoned)
      (tm1 status-abandoned)
      &optional ➜
      )
    (declare (ignore tm0 tm1 ➜))
    (operation-on-abandoned)
    )

  (def-abandoned-1 a◨ instance)
