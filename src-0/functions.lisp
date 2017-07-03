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
  (defun cant-happen (&rest x) (declare (ignore x)) (error 'cant-happen))
  (defun alloc-fail () (error 'alloc-fail)) ;; for memory allocation failure
  (defun not-implemented () (error 'not-implemented)) ;; for memory allocation failure

  ;; status related
  (defun operation-on-abandoned () (error 'operation-on-abandoned))
  (defun accessed-empty () (error 'accessed-empty))
  (defun access-through-parked-head () (error 'access-through-parked-head))

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
                 l
                 ))
             )
      (remove-key-pair-1 l)
      ))


  ;; remove multiple keys pairs.  go-away-keys are consumed as matched
  ;; some examples:
  ;;
  ;; * (remove-key-pairs {:a 1 :b 2 :a 3} {:a :a})
  ;;   (:B 2)
  ;; * (remove-key-pairs {:a 1 :b 2 :a 3} {:a})
  ;;   (:B 2 :A 3)
  ;; * (remove-key-pairs {:a 1 :b 2 :a 3} {:a :a :b})
  ;;   NIL
  ;; * (remove-key-pairs {:a 1 :b 2 :a 3} {:b :a})
  ;;   (:A 3)
  ;;
    (defun consume-key (go-away-key key-list)
      (let(
            (found ∅)
            )
        (labels(
                 (consume-key-1 (key-list)
                   (let(
                         (a-key (car key-list))
                         (rest-keys  (cdr key-list))
                         )
                     (cond
                       ((eq go-away-key a-key) (setf found t) rest-keys)
                       ((¬ rest-keys) key-list)
                       (t (cons a-key (consume-key-1 rest-keys)))
                       )))
                 )
          (values (consume-key-1 key-list) found)
          )))

    (defun remove-key-pairs (l go-away-keys)
      (if (∧ l (cdr l))
        (let(
              (key (car l))
              (val (cadr l))
              )
          (multiple-value-bind (pruned-go-away-keys found) (consume-key key go-away-keys)
            (if found
              (remove-key-pairs (cddr l) pruned-go-away-keys)
              (cons key (cons val (remove-key-pairs (cddr l) pruned-go-away-keys)))
              )))
        l
        ))


