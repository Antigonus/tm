#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Some fundamental functions.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; basic functions
;;
  (defun boolify (b) (not (not b)))

  (defun do-nothing (&rest x) (declare (ignore x))(values))

  ;; common lisp already has an 'identity with a slightly different definition
  (defun echo (&rest x) (apply #'values x))

  ;; e.g. (be 5) returns a function that when called returns 5, etc:
  ;; the returned function igonres arguments .. probably should make this
  ;; function a bit more sophisticated
  ;; note that be called without args is equivalent to do #'do-nothing
  ;;
    (defun be (&rest it) 
      (λ (&rest args)(declare (ignore args))(apply #'values it))
      )

  (defun notλ (f) (λ(&rest x)(not (apply f x))))


