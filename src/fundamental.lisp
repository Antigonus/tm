#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
|#

(in-package #:le)


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

