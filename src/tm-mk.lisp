#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make machines from other objects.
  Make other objects from machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; the base type
;;
  (defclass tape-machine ()
    (
      (HA 
        :initarg :HA 
        :accessor HA
        )
      (tape
        :initarg :tape
        :accessor tape
        )
      ))

;;--------------------------------------------------------------------------------
;; make a tape machine from another form
;;
  (defvar *mk-tm-hash* (make-hash-table :test 'eq))

  (defun mk-tm-hook (type mk-function)
    "Function takes three arguments, init cont-ok cont-fail.  
     Late type will be used to lookup and call the function.
     "
    (setf (gethash type *mk-tm-hash*) mk-function)
    )

  (defun mk-tm 
    (
      type
      &optional 
      init
      (cont-ok #'echo) 
      (cont-fail 
        (λ() (error 'mk-tm-bad-init-type :text "unrecognized init type") ∅)
        )
      )
    (multiple-value-bind 
      (function lookup-success) 
      (gethash type *mk-tm-hash*)
      (if
        lookup-success
        (funcall function init cont-ok cont-fail)
        (funcall cont-fail)
        )
      ))

;;--------------------------------------------------------------------------------
;; converts a tape machine to another form
;;
  (defgeneric to-sequence(tm))
  (defgeneric to-list (tm))
  (defgeneric to-extendable-array (tm))
  (defgeneric to-array (tm))
