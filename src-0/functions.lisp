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

;;--------------------------------------------------------------------------------
;; fundamental errors as functions
;;
  (defun cant-happen () (error 'impossible-to-get-here))
  (defun alloc-fail () (error 'alloc-fail)) ;; for memory allocation failure

;;--------------------------------------------------------------------------------
;; pass a value in a box  (via dmitry_vk stack exchange)
;;
;;  Normally one would use a closure to bring varibles into scope, instead of the argument
;;  list. I.e. one would use 'let over lambda', (or least let over labels) and this is in
;;  fact what this is doing, though in a convoluted manner.
;;
  (defstruct box read write)

  (defmacro box (arg)
    (let(
          (new-value (gensym))
          )
      `(make-box 
         :read (lambda () ,arg)
         :write (lambda (,new-value) (setf ,arg ,new-value))
         )))
                
  (defun unbox (a-box)
    (funcall (box-read a-box))
    )

  (defun (setf unbox) (new-value a-box)
    (funcall (box-write a-box) new-value)
    )

;;--------------------------------------------------------------------------------
;; remove a key pair
;;

  ;; removes the first key pair from the list (leaves later ones)
  (defun remove-key-pair (l go-away-key)
    (labels(
             (remove-key-pair-1 (l)
               (if (∧ l (cdr l))
                 (let(
                       (key (car l))
                       (val (cadr l))
                       )
                   (if (eq go-away-key key)
                     (cddr l)
                     (cons key (cons val (remove-key-pair-1 (cddr l))))
                     ))
                 (if l l ∅)
                 ))
             )
      (remove-key-pair-1 l)
      ))

