#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt
  
|#

(in-package #:tm)

;;defsynonym from "Successful Lisp"
(defmacro defsynonym (old-name new-name)
  "Define OLD-NAME to be equivalent to NEW-NAME when used in the first position of a Lisp form."
  `(defmacro ,new-name (&rest args) `(,',old-name ,@args))
  )

;; unicode support
;;
  (defconstant ∅ nil)

  (defsynonym /= ≠)
  (defsynonym <= ≤)
  (defsynonym >= ≥)

  (defsynonym not ¬)
  (defsynonym and ∧)
  (defsynonym or ∨)

  (defsynonym string/= string≠)
  (defsynonym string<= string≤)
  (defsynonym string>= string≥)

  (defsynonym lambda λ)

;; sorry LISP, but some of these names are a tad bit too long for my taste
;;
  (defsynonym defparameter defparam)

;; prints a new line
;;
  (defun nl (&optional (stream t))
    "prints a new line"
    (format stream "~%")
    )

