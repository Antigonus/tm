#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch

|#

(in-package #:le)

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



