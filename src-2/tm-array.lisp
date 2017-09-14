#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Implementation of a tape machine over a tape-array

|#


(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  (def-type tm-array (tm)
    (
      (head
        :initform ∅
        :accessor head
        )
      (tape 
        :initform ∅
        :accessor tape
        )
      (channel ; when we fork a tape array, we make a complete copy
        :initform 0
        :accessor channel
        )
      ))

  (def-type tm-array-abandoned (tm-array tm-abandoned)()) ; used by scoping operators

  ;; useful conjunctions of status:
  (def-type tm-array-empty-parked-active (tm-array tm-empty-parked-active)()) ; not abandoned
  (def-type tm-array-empty-parked        (tm-array tm-empty-parked)()) ; not active
  (def-type tm-array-parked-active       (tm-array tm-parked-active)()) ; not empty

  (def-type tm-array-empty
    (
      tm-array-empty-parked-active
      tm-array-empty-parked
      )
    (tm-empty)
    )
  (def-type tm-array-parked
    (
      tm-array-empty-parked-active
      tm-array-empty-parked
      tm-array-parked-active
      )
    (tm-parked)
    )
  (def-type tm-array-active
    (
      tm-array-empty-parked-active
      tm-array-parked-active
      )
    (tm-active)
    )
  
  (defun-typed to-abandoned ((tm tm-array)) (change-class tm 'tm-array-abandoned))
  (defun-typed to-empty     ((tm tm-array)) (change-class tm 'tm-array-empty))
  (defun-typed to-parked    ((tm tm-array)) (change-class tm 'tm-array-parked))
  (defun-typed to-active    ((tm tm-array)) (change-class tm 'tm-array-active))

  (defun-typed init ((tm tm-array) &optional ➜)
    (destructuring-bind
      (
        &key
        array 
        &allow-other-keys
        )
      ➜
      (setf (head tm) ∅)
      (setf (tape tm) array)
      ))


;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defun-typed p ((tm tm-array-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (head tm) ∅) 
      (to-parked tm)
      [➜ok]
      ))

  ;; cue the head
  (defun-typed u ((tm tm-array-parked-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0) ; for higher rank tapes, the user will have to specify address to cue to
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (head tm) address)
      [➜ok]
      ))

  ;; stepping from parked cues the machine
  (defun-typed s ((tm tm-array-parked) &optional ➜)
    (u tm)
    )
 
  (defun-typed s ((tm tm-array-active) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok (be t))
        (➜bound (be ∅))
        &allow-other-keys
        )
      ➜
      (cond
        ((< (head tm) (max<tape-array> (tape tm))) ; tape is never empty for an active machine
          (incf (head tm))
          [➜ok]
          )
        (t
          [➜bound]
          ))))
      
  ;; returns a function that steps by a stride
  (def-typed s-stride (stride &optional ➜)
  (defun-typed s-stride ((stride number) &optional ➜)
    (λ(tm &optional ➜)
      (cond
        ((≤ (head tm) (- (max<tape-array> (tape tm)) stride)) ; tape is never empty for an active machine
          (setf (head tm) (+ (head tm) stride))
          [➜ok]
          )
        (t
          [➜bound]
          ))))
  
  ;; invokes boundaries, note the option to specify the step function
  (def-function-class s* (tm &optional ➜)
    (:documentation
      "Steps the head until the ➜bound continuation is taken.
       "))
  (defun-typed s* ((tm tm-array-abandoned) &optional ➜)
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
  (defun-typed ∃ ((tm tm-array-abandoned) pred &optional ➜)
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed ∃ ((tm tm-array-empty) (pred function) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      [➜∅]
      ))
  (defun-typed ∃ ((tm tm-array-parked) (pred function) &optional ➜)
    (destructuring-bind
      (&key
        (cue #'s) ; all such #'s must unpark the head on a parked machine
        &allow-other-keys
        )
      ➜
      [cue tm ; unpark
        {
          :➜ok (λ()(∃ tm pred ➜))
          (o ➜)
          }]))
  (defun-typed ∃ ((tm tm-array-active) (pred function) &optional ➜)
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
  (defun-typed ∀ ((tm tm-array-abandoned) pred &optional ➜) 
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed ∀ ((tm tm-array-empty-parked-active) pred &optional ➜)
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

  ;; cues the head before calling quantifier
  ;; note that #'p is a cueing function
  (def-function-class u∃ (tm pred &optional ➜))
  (defun-typed u∃ ((tm tm-array-abandoned) pred &optional ➜) 
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed u∃ ((tm tm-array-empty-parked-active) pred &optional ➜)
    (destructuring-bind
      (&key
        (cue #'u)
        &allow-other-keys
        )
      ➜
      [cue tm
        {:➜ok (∃ tm pred ➜)}
        (o ➜)
        ]))
  (def-function-class u∀ (tm pred &optional ➜))
  (defun-typed u∀ ((tm tm-array-abandoned) pred &optional ➜) 
    (declare (ignore tm pred ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed u∀ ((tm tm-array-empty-parked-active) pred &optional ➜)
    (destructuring-bind
      (&key
        (cue #'u)
        &allow-other-keys
        )
      ➜
      [cue tm
        {:➜ok (∀ tm pred ➜)}
        (o ➜)
        ]))
