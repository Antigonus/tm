#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Init binds to a sequence.

Currently this is being used to create a tape so that other tapes can initialize
from a sequence.  

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;

  (def-type cell-sequence (cell)
    (
      (tape
        :initarg :tape
        :accessor tape
        )
      (index
        :initarg :index
        :accessor index
        )))

  (def-type tape-sequence (tape)
    (
      (the-sequence
        :initarg :the-sequence
        :accessor the-sequence
        )))
  (def-type tape-sequence-empty (tape-sequence tape-empty)())
  (def-type tape-sequence-active (tape-sequence tape-active)())
  (defun-typed to-active ((tape tape-sequence)) (change-class tape 'tape-sequence-active))
  (defun-typed to-empty  ((tape tape-sequence)) (change-class tape 'tape-sequence-empty))

  ;; binds tape to seq
  (defun-typed init ((tape tape-sequence) (seq sequence) &optional ➜)
    (destructuring-bind
      (&key
        ;; parms ; to pass a parms in we need a parms dictionary to look the name up ..
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (cond
        ((∨ (¬ seq) (= 0 (length seq)))
          (to-empty tape)
          )
        (t
          (setf (the-sequence tape) seq)
          (to-active tape)
          ))
      [➜ok tape]
      ))

  ;; provides sequence intialization for other tape types:
  (defun-typed init ((tape-1 tape) (seq sequence) &optional ➜)
    (mk 'tape-sequence seq
      {
        :➜ok (λ(tape-0) (init tape-1 tape-0 ➜))
        }))
               

;;--------------------------------------------------------------------------------
;; tape queries
;;
  (defun-typed =<cell> ((cell-0 cell-sequence) (cell-1 cell-sequence) &optional ➜)
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

  (defun-typed r<cell> ((cell cell-sequence))
    (let(
          (the-sequence (the-sequence (tape cell)))
          (index (index cell))
          )
      (elt the-sequence index)
      ))

  (defun-typed w<cell> ((cell cell-sequence) instance)
    (let(
          (the-sequence (the-sequence (tape cell)))
          (index (index cell))
          )
      (setf (elt the-sequence index) instance)
      ))

  (defun-typed left-bound ((tape tape-sequence) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok
        (make-instance 'cell-sequence :tape tape :index 0)
        ]))

  (defun-typed right-bound ((tape tape-sequence) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok
        (make-instance 'cell-sequence 
          :tape tape
          :index (1- (length (the-sequence tape)))
          )]))
      
  (defun-typed right-neighbor ((cell cell-sequence) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜right-bound (λ()(error 'step-from-right-bound)))
        &allow-other-keys
        )
      ➜
      (let*(
             (tape  (tape  cell))
             (index (index cell))
             (maxdex (1- (length (the-sequence tape))))
             )
        (cond
          ((< index maxdex)
            [➜ok (make-instance 'cell-sequence :tape tape :index (1+ index))]
            )
          (t
            [➜right-bound]
            )))))

  (defun-typed left-neighbor ((cell cell-sequence) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜left-bound (λ()(error 'step-from-left-bound)))
        &allow-other-keys
        )
      ➜
      (let*(
             (tape  (tape  cell))
             (index (index cell))
             )
        (cond
          ((> 0 index)
            [➜ok (make-instance 'cell-sequence :tape tape :index (1- index))]
            )
          (t
            [➜left-bound]
            )))))


;;--------------------------------------------------------------------------------
;; length-tape
;;
 (defun-typed maximum-address ((tape tape-sequence-active) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (1- (length (the-sequence tape)))]
      ))
