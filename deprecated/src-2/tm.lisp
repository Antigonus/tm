#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Architectural definition of Tape Machine. I.e. the tape machine interface.

  Psuedo Status: 
     abandoned - the tape machine has been left for garbage collection, using it in any manner
                 raises 'use-of-abandoned

  Status:
     empty - the tape is empty
     parked - the tape is not empty, but the head does not indicate a cell on the tape
     active - tape is not empty, head is on the tape

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tm ()())

  (def-type tm-abandoned (tm)()) ; used by scoping operators

  ;; useful conjunctions of status:
  (def-type tm-empty-parked-active (tm)()) ; not abandoned
  (def-type tm-empty-parked (tm)()) ; not active
  (def-type tm-parked-active (tm)()) ; not empty

  (def-type tm-empty ; empty tape, the head is not on any cell
    (
      tm-empty-parked-active
      tm-empty-parked
      )
    ()
    )
  (def-type tm-parked ; non-empty tape, the head is not on any cell
    (
      tm-empty-parked-active
      tm-empty-parked
      tm-parked-active
      )
    ()
    )
  (def-type tm-active ; non-empty tape, and the head is on a cell
    (
      tm-empty-parked-active
      tm-parked-active
      )
    ()
    )
  
  ;; changing the machine status, these are private
  (def-function-class to-abandoned (tm))
  (def-function-class to-active (tm))
  (def-function-class to-empty (tm))
  (def-function-class to-parked (tm))

  (defun-typed to-abandoned ((tm tm)) (change-class tm 'tm-abandoned))
  (defun-typed to-empty     ((tm tm)) (change-class tm 'tm-empty))
  (defun-typed to-parked    ((tm tm)) (change-class tm 'tm-parked))
  (defun-typed to-active    ((tm tm)) (change-class tm 'tm-active))

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (def-function-class p (tm &optional ➜)
    (:documentation
      "Parks the head.
       "))
  ;; an empty machine is already parked
  (defun-typed p ((tm tm-empty-parked) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      [➜ok]
      ))
  (defun-typed p ((tm tm-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (to-parked tm)
      [➜ok]
      ))

  (def-function-class u (tm &optional ➜)
    (:documentation
      "Cue the head to specific cell.
       "))
  (defun-typed u ((tm tm-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed u ((tm tm-empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜bound (be ∅))
        &allow-other-keys
        )
      ➜
      [➜bound]
      ))

  ;; step, user will sometimes provide their own step function
  (def-function-class s (tm &optional ➜)
    (:documentation
      "Steps the head to a neighboring cell.  :address choses the neighbor, defaults
        to 0 (right).
       "))
  (defun-typed s ((tm tm-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed s ((tm tm-empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜bound (be ∅))
        &allow-other-keys
        )
      ➜
      [➜bound]
      ))
  
  ;; invokes boundaries, note the option to specify the step function
  (def-function-class s* (tm &optional ➜)
    (:documentation
      "Steps the head until the ➜bound continuation is taken.
       "))
  (defun-typed s* ((tm tm-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (error 'use-of-abandoned)
    )

  ;; a provided step function might 'step by zero' so we don't have universal bound invocation behavior
  ;; when stepping an empty machine (step by zero is ➜ok, otherwise ➜bound)


;;--------------------------------------------------------------------------------
;; quantification
;;
;;    pred takes three arguments, a tape machine, a false continuation, and a true
;;    continuation.  false is normally 0, and true 1,  so the false clause come first.
;;    Optionally provide a step function.
;;
  (def-function-class ∃ (tm pred &optional ➜))
  (defun-typed ∃ ((tm tm-abandoned) pred &optional ➜)
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed ∃ ((tm tm-empty) (pred function) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
  (defun-typed ∃ ((tm tm-parked) (pred function) &optional ➜)
    (destructuring-bind
      (&key
        (first #'s) ; must unpark the head on a parked machine
        &allow-other-keys
        )
      ➜
      [first tm ; unpark
        {
          :➜ok (λ()(∃ tm pred ➜))
          (o ➜)
          }]))
  (defun-typed ∃ ((tm tm-active) (pred function) &optional ➜)
    (destructuring-bind
      (&key
        (step #'s) ; steps the head
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (⟳(λ(again)
          [pred tm (λ()[step tm {:➜ok again :➜bound ➜∅}]) ➜t]
          ))))
      
  (def-function-class ∀ (tm pred &optional ➜))
  (defun-typed ∀ ((tm tm-abandoned) pred &optional ➜) 
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed ∀ ((tm tm-empty-parked-active) pred &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
    (∃ 
      tm
      (λ(tm c∅ ct)[pred tm ct c∅])
      {:➜∅ ➜t :➜t ➜∅ (o ➜)}
      )))

  ;; parks the head before calling quantifier
  (def-function-class p∃ (tm pred &optional ➜))
  (defun-typed p∃ ((tm tm-abandoned) pred &optional ➜) 
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed p∃ ((tm tm-empty-parked-active) pred &optional ➜)
    (p tm
      {:➜ok (∃ tm pred ➜)}
      (o ➜)
      ))
  (def-function-class p∀ (tm pred &optional ➜))
  (defun-typed p∀ ((tm tm-abandoned) pred &optional ➜) 
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed p∀ ((tm tm-empty-parked-active) pred &optional ➜)
    (p tm
      {:➜ok (∀ tm pred ➜) }
      (o ➜)
      ))
