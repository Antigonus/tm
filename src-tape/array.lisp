#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Init creates an array, shallow copies from another tape.

  A tape space implemented over an array. Each cell holds an array element.

  Topology modification is not supported.


|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;

  ;; If this were C we could have an actual cell rather than emulating one here.
  ;; Accordingly, instead of the index we would have a reference, then dereference it to
  ;; do a read.  Though we would still need to know the max index to handle the
  ;; bound-right endcase on a right-neighbor call.
  (def-type cell-array (cell)
    (
      (tape
        :initarg :tape
        :accessor tape
        )
      (index
        :initarg :index
        :accessor index
        )
      ))

  ;; potentially shared by a number of tape-arrays
  ;; stuff one would like to see compiled away, would like to put in sym table instead
  (def-type tape-array-parms ()
    (
      (name ; name for the parms
        :initarg :name
        :accessor name
        )
      (maxdex ; maximum index into the array
        :initarg :maxdex
        :accessor maxdex
        )
      (element-length ;; ingnored at the moment, our arrays currently only hold references
        :initarg :element-length
        :accessor element-length
        )
      ))

  (def-type tape-array (tape)
    (
      (the-array
        :initarg :the-array
        :accessor the-array
        )
      (parms
        :initarg :parms
        :accessor parms
        )
      ))

  (def-type tape-array-active (tape-array tape-active)())
  (def-type tape-array-empty (tape-array tape-empty)())
  (defun-typed to-active ((tape tape-array)) (change-class tape 'tape-array-active))
  (defun-typed to-empty  ((tape tape-array)) (change-class tape 'tape-array-empty))

  (defun tape-array-make (tape maximum-address)
    (setf
      (parms tape)
      (make-instance
        'tape-array-parms
        :name 'local
        :maxdex maximum-address
        ))
    (setf
      (the-array tape)
      (make-array
        (1+ maximum-address)
        :element-type t
        :adjustable ∅
        :fill-pointer ∅
        :displaced-to ∅
        ))
    )
                
  (defun-typed init ((tape-1 tape-array) (tape-0 tape-empty) &optional ➜)
    (destructuring-bind
      (&key
        initial-element 
        maximum-address
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (cond
        (maximum-address
          (tape-array-make tape-1 maximum-address)
          (do
            ((i 0 (1+ i)))
            ((> i maximum-address))
            (setf (aref (the-array tape-1) i) initial-element)
            )
          (to-active tape-1)
          )
        (t
          (to-empty tape-1)
          ))
      [➜ok tape-1]
      ))

  ;; shallow copy from another tape
  (defun-typed init ((tape-1 tape-array) (tape-0 tape-active) &optional ➜)
    (destructuring-bind
      (&key
        initial-element 
        maximum-address
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (when (¬ maximum-address) (setf maximum-address (maximum-address tape-0)))
      (tape-array-make tape-1 maximum-address)
      (to-active tape-1)
      (shallow-copy-no-topo tape-1 tape-0 {:initial-element initial-element})
      [➜ok tape-1]
      ))


;;--------------------------------------------------------------------------------
;; accessing instances
;;
  (defun-typed ◧snr (address (tape tape-array-active)  &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜bound-left (λ()(error 'step-from-bound-left)))
        (➜bound-right (λ()(error 'step-from-bound-right)))
        &allow-other-keys
        )
      ➜
      (let(
            (the-array (the-array tape))
            (maximum-address (maximum-address tape))
            )
        (cond
          ((< address 0)
            [➜bound-left]
            )
          ((≤ address maximum-address)
            [➜ok (aref the-array address)]
            )
          (t
            [➜bound-right]
            )
          )
        )))

  (defun-typed ◧snw (address (tape tape-array-active) instance  &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜bound-left (λ()(error 'step-from-bound-left)))
        (➜bound-right (λ()(error 'step-from-bound-right)))
        &allow-other-keys
        )
      ➜
      (let(
            (the-array (the-array tape))
            (maximum-address (maximum-address tape))
            )
        (cond
          ((< address 0)
            [➜bound-left]
            )
          ((≤ address maximum-address)
            (setf (aref the-array address) instance)
            [➜ok]
            )
          (t
            [➜bound-right]
            )
          )
        )))

;;--------------------------------------------------------------------------------
;; tape queries
;;
  (defun-typed =<cell> ((cell-0 cell-array) (cell-1 cell-array) &optional ➜)
    (destructuring-bind
      (&key
        (➜∅ (be ∅))
        (➜t (be t))
        &allow-other-keys
        )
      ➜
      (if
        (∧
          (eq  (tape cell-0)  (tape cell-1))
          (eql (index cell-0) (index cell-1))
          )
        [➜t]
        [➜∅]
        )))

  (defun-typed r<cell> ((cell cell-array))
    (let(
          (the-array (the-array (tape cell)))
          (index (index cell))
          )
      (aref the-array index)
      ))

  ;; Writing a zero into the bound-right tile makes the natural shorter.  But this is a cell
  ;; operation not a tape operation, so the outer tape operation will have to take this
  ;; into account.
  (defun-typed w<cell> ((cell cell-array) instance)
    (let(
          (the-array (the-array (tape cell)))
          (index (index cell))
          )
      (setf (aref the-array index) instance)
      ))

  (defun-typed bound-left ((tape tape-array-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok 
        (make-instance 
          'cell-array
          :tape tape
          :index 0
          )]
      ))

  (defun-typed right-neighbor ((cell cell-array) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜bound-right (λ()(error 'step-from-bound-right)))
        &allow-other-keys
        )
      ➜
      (let*(
             (tape  (tape  cell))
             (index (index cell))
             (parms (parms tape))
             (maxdex (maxdex parms))
             )
        (cond
          ((< index maxdex)
            [➜ok (make-instance 'cell-array :tape tape :index (1+ index))]
            )
          (t
            [➜bound-right]
            )))))


;;--------------------------------------------------------------------------------
;; topology manipulation
;;   no topology manipulation allowed for array
;; 


;;--------------------------------------------------------------------------------
;; length-tape
;;
   (defun-typed maximum-address ((tape tape-array-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (maxdex (parms tape))]
      ))
