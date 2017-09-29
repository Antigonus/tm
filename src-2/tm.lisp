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

move is topological
copy is not - we don't need copy as we have read and write

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
;; tape operations
;;
  (def-function-class eur (tm &optional ➜)
    (:documentation
      "Reads the cell at :address.  :address defaults to 0.
       "))
  (defun-typed eur ((tm tm-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed eur ((tm tm-empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  (def-function-class euw (tm instance &optional ➜)
    (:documentation
      "Writes the cell at :address. :address defaults to 0.
       "))
  (defun-typed euw ((tm tm-abandoned) instance &optional ➜)
    (declare (ignore tm instance ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed euw ((tm tm-empty) instance &optional ➜)
    (declare (ignore tm instance))
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))

  (def-function-class eus*-count (tm instance &optional ➜)
    (:documentation
      "Writes the cell at :address. :address defaults to 0.
       "))
  (defun-typed eus*-count ((tm tm-abandoned) instance &optional ➜)
    (declare (ignore tm instance ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed eus*-count ((tm tm-empty) instance &optional ➜)
    (declare (ignore tm instance))
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))


;;;


  (def-function-class epa (tm &optional ➜)
    (:documentation
      "Prepends a new leftmost cell or cells. 
      :instance is a new instance.
      :fill  
       "))

  (def-function-class ep-a (tm instance &optional ➜)
    (:documentation
      "Appends a new rightmost cell or cells.
       "))

  (def-function-class ep-a (tm instance &optional ➜)
    (:documentation
      "Appends a new rightmost cell or cells.
       "))


  (def-function-class epd (tm &optional ➜)
    (:documentation
      "Deletes the leftmost cell. Returns the instance.
       "))

  (def-function-class ep-d (tm &optional ➜)
    (:documentation
      "Deletes the rightmost cell.
       "))


;;--------------------------------------------------------------------------------
;; absolute head control
;;

  ;; cue - functions that use cue will accept a cue function parameter so the user may use their own
  ;; defaults to the leftmost of the tape
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

  (defun-typed abandon ((tm tm))
    (to-abandoned tm)
    )
  (defun-typed abandon ((tm tm-abandoned)))


  (def-function-class @ (tm &optional ➜)
    (:documentation
      "Location of the head as a natural number, or list of natural numbers.
       "))
  (defun-typed @ ((tm tm-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed @ ((tm tm-empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed @ ((tm tm-parked) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜parked (λ()(error 'accessed-parked)))
        &allow-other-keys
        )
      ➜
      [➜parked]
      ))

  (def-function-class max@ (tm &optional ➜)
    (:documentation
      "Maximum address in the tape's address space.
       "))
  (defun-typed max@ ((tm tm-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed max@ ((tm tm-empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed max@ ((tm tm-parked) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜parked (λ()(error 'accessed-parked)))
        &allow-other-keys
        )
      ➜
      [➜parked]
      ))

  (def-function-class max-active@ (tm &optional ➜)
    (:documentation
      "Maximum address in the tape's address space.
       "))
  (defun-typed max-active@ ((tm tm-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed max-active@ ((tm tm-empty) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed max-active@ ((tm tm-parked) &optional ➜)
    (declare (ignore tm))
    (destructuring-bind
      (
        &key
        (➜parked (λ()(error 'accessed-parked)))
        &allow-other-keys
        )
      ➜
      [➜parked]
      ))



;;--------------------------------------------------------------------------------
;; relative head control
;;
  ;; step, user will sometimes provide their own step function
  (def-function-class s (tm &optional ➜)
    (:documentation
      "Steps the head to a neighboring cell.
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
  
;;--------------------------------------------------------------------------------
;; access through head
;;
  (def-function-class r (tm &optional ➜)
    (:documentation
      "Reads the cell under the head.
       "))
  (defun-typed r ((tm tm-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed r ((tm tm-empty) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed r ((tm tm-parked) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜parked (λ()(error 'accessed-parked)))
        &allow-other-keys
        )
      ➜
      [➜parked]
      ))

  (def-function-class w (tm instance &optional ➜)
    (:documentation
      "Reads the cell under the head.
       "))
  (defun-typed w ((tm tm-abandoned) instance &optional ➜)
    (declare (ignore tm instance ➜))
    (error 'use-of-abandoned)
    )
  (defun-typed w ((tm tm-empty) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed w ((tm tm-parked) instance &optional ➜)
    (destructuring-bind
      (
        &key
        (➜parked (λ()(error 'accessed-parked)))
        &allow-other-keys
        )
      ➜
      [➜parked]
      ))

