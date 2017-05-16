#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; new function classes for status
;;
  (def-function-class hp∃ (tm pred &optional ➜))
  (def-function-class hp∀ (tm pred &optional ➜))
  (def-function-class hp∃* (tm pred))
  (def-function-class hp∀* (tm function))

;;--------------------------------------------------------------------------------
;; abandoned
;;
  (defun-typed ∃ ((tm abandoned) pred &optional ➜)
    (declare (ignore tm pred ➜))
    (operation-on-abandoned)
    )
  (defun-typed h◧∃ ((tm abandoned) pred &optional ➜)
    (declare (ignore tm pred ➜))
    (operation-on-abandoned)
    )
  (defun-typed hp∃ ((tm abandoned) pred &optional ➜)
    (declare (ignore tm pred ➜))
    (operation-on-abandoned)
    )

  (defun-typed ∀ ((tm abandoned) pred &optional ➜)
    (declare (ignore tm pred ➜))
    (operation-on-abandoned)
    )
  (defun-typed h◧∀ ((tm abandoned) pred &optional ➜)
    (declare (ignore tm pred ➜))
    (operation-on-abandoned)
    )
  (defun-typed hp∀ ((tm abandoned) pred &optional ➜)
    (declare (ignore tm pred ➜))
    (operation-on-abandoned)
    )

  (defun-typed ∃* ((tm abandoned) pred)
    (declare (ignore tm pred))
    (operation-on-abandoned)
    )
  (defun-typed h◧∃* ((tm abandoned) pred)
    (declare (ignore tm pred))
    (operation-on-abandoned)
    )
  (defun-typed hp∃* ((tm abandoned) pred)
    (declare (ignore tm pred))
    (operation-on-abandoned)
    )

  (defun-typed ∀* ((tm abandoned) function)
    (declare (ignore tm function))
    (operation-on-abandoned)
    )
  (defun-typed h◧∀* ((tm abandoned) function)
    (declare (ignore tm function))
    (operation-on-abandoned)
    )
  (defun-typed hp∀* ((tm abandoned) function)
    (declare (ignore tm function))
    (operation-on-abandoned)
    )


;;--------------------------------------------------------------------------------
;; empty
;;
  ;; no existence case can be found
  (defun-typed ∃ ((tm empty) pred &optional ➜)
    (declare (ignore tm pred))
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
  ;; no existence case can be found independent of head initialization
  (defun-typed h◧∃ ((tm empty) pred &optional ➜)
    (declare (ignore tm pred))
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
  (defun-typed hp∃ ((tm empty) pred &optional ➜)
    (declare (ignore tm pred))
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))

  ;; we can not find a case where existence is false
  ;; .. there are zero cases where existence should be checked
  (defun-typed ∀ ((tm empty) pred &optional ➜)
    (declare (ignore tm pred))
    (destructuring-bind
      (&key
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      [➜t]
      ))
  (defun-typed h◧∀ ((tm empty) pred &optional ➜)
    (declare (ignore tm pred))
    (destructuring-bind
      (&key
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      [➜t]
      ))
  (defun-typed hp∀ ((tm empty) pred &optional ➜)
    (declare (ignore tm pred))
    (destructuring-bind
      (&key
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      [➜t]
      ))
  
  (defun-typed ∃* ((tm empty) pred)
    (declare (ignore tm pred))
    (cons 0 0)
    )
  (defun-typed h◧∃* ((tm empty) pred)
    (declare (ignore tm pred))
    (cons 0 0)
    )
  (defun-typed hp∃* ((tm empty) pred)
    (declare (ignore tm pred))
    (cons 0 0)
    )

  (defun-typed ∀* ((tm empty) function)
    (declare (ignore tm function))
    (values)
    )
  (defun-typed h◧∀* ((tm empty) function)
    (declare (ignore tm function))
    (values)
    )
  (defun-typed hp∀* ((tm empty) function)
    (declare (ignore tm function))
    (values)
    )

;;--------------------------------------------------------------------------------
;; parked
;;
  (defun-typed ∃ ((tm parked) pred &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [pred tm ➜t (λ()(h◧∃ tm pred {:➜t ➜t :➜∅ ➜∅}))]
      ))
  (defun-typed hp∃ ((tm parked) pred &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (∃ tm {:➜t ➜t :➜∅ ➜∅})
      ))

  (defun-typed ∀ ((tm parked) pred &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [pred tm (λ()(h◧∀ tm pred {:➜t ➜t :➜∅ ➜∅})) ➜∅]
      ))
  (defun-typed hp∀ ((tm parked) pred &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (∀ tm {:➜t ➜t :➜∅ ➜∅})
      ))

  (defun-typed ∃* ((tm parked) pred)
    [pred tm 
      (λ()
        (let(
              (counts (h◧∃* tm pred))
              )
          (cons (1+ (car counts)) (1+ (cdr counts)))
          ))
      (λ()
        (let(
              (counts (h◧∃* tm pred))
              )
          (cons (car counts) (1+ (cdr counts)))
          ))
      ])

  (defun-typed hp∃* ((tm parked) pred)
    (∃* tm pred)
    )

  (defun-typed ∀* ((tm parked) function)
    [function tm]
    (h◧∀* tm function)
    )
  (defun-typed hp∀* ((tm parked) function)
    (∀* tm function)
    )


;;--------------------------------------------------------------------------------
;; active
;;
  (defun-typed hp∃ ((tm active) pred &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (hp tm)
      (∃ tm {:➜t ➜t :➜∅ ➜∅})
      ))
  (defun-typed hp∀ ((tm active) pred &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (hp tm)
      (∀ tm {:➜t ➜t :➜∅ ➜∅})
      ))
  (defun-typed hp∃* ((tm active) pred)
    (hp tm)
    (∃* tm pred)
    )
  (defun-typed hp∀* ((tm active) function)
    (hp tm)
    (∀* tm function)
    )


