#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Interpret a number as a sequence of tiles. Later we will have a tiled number type,
  so one should not assume that this will always be a bit field implementation.

  Yikes, this would be so much more efficient written custom in assembly.  Perhaps this
  can drive a test bench.

  In SBLC (byte width position) is just a cons cell.  So there isn't much performance
  advantage to making the head a bit field instead of a position locator.

  seems we should have another continuation on write for when the instance is not
  the same length as the tile.


|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;

;; this tm walks the tiles in an address
(def-type constant-length-array-tm (tape-machine)
  (
    (head
      :initarg :head 
      :accessor head
      )
    (max 
      :initarg :max
      :accessor max
      )
    (tape
      :initarg :tape
      :accessor tape
      )))

;;--------------------------------------------------------------------------------
;;  Make constant-length-array machines. A constant length array must be specified with a
;;  maximum index. The tape parameter is interpreted as initialization data.
;;
  (defun-typed init 
    (
      (tm constant-length-array-tm)
      &optional 
      keyed-parms
      ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜fail (λ()(error 'bad-init-value)))
        &allow-other-keys          
        )
      ➜
      (destructuring-bind
        (&key tape length initial-element element-type) keyed-parms
        (setf (head tm) 0)
        (cond
          ((∧ length (numberp length) (> 0 length))
            (setf (tape tm)
              (make-array length
                :element-type (if element-type element-type t)
                :initial-element initial-element
                :adjustable ∅
                :fill-pointer ∅
                :displaced-to ∅
                )
            (call-next-method)
            [➜ok tm]
            )
          (t
            [➜fail]
            ))))))

;;--------------------------------------------------------------------------------
;; location
;;  
  (defun-typed on-leftmost ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= (head tm) 0) [➜t] [➜∅])
      ))

  (defun-typed on-rightmost ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (= (head tm) (max tm)) [➜t] [➜∅])
      ))

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defun-typed r ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (aref (tape tm) (head tm))]
      ))

  (defun-typed esr ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (on-rightmost tm
        {
          :➜t ➜rightmost
          :➜∅ (λ()
                [➜ok  (aref (tape tm) (1+ (head tm)))]
                )
          })))

  (defun-typed w ((tm constant-length-array-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (aref (tape tm) (head tm)) instance)
      [➜ok]
      ))

  (defun-typed esw ((tm constant-length-array-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost #'echo)
        &allow-other-keys
        )
      ➜
      (on-rightmost tm
        {
          :➜t ➜rightmost
          :➜∅ (λ()
                (setf (aref (tape tm) (1+ (head tm))) instance)
                [➜ok]
                )
          })))

  (defun-typed ◧r ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        )
      ➜
      [➜ok (aref (tape tm) 0)]
      ))

  (defun-typed ◧sr ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜rightmost (λ()(error 'step-from-rightmost)))
        &allow-other-keys
        )
      ➜
      (if
        (≤ (max tm) 0)
        [➜rightmost]
        [➜ok (aref (tape tm) 1)]
        )))

  (defun-typed ◧w ((tm constant-length-array-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (aref (tape tm) 0) instance)
      [➜ok]
      ))

  (defun-typed ◧sw ((tm constant-length-array-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (if
        (≤ (max tm) 0)
        [➜rightmost]
        [➜ok (setf (aref (tape tm) 1) instance)]
        )))


;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defun-typed -s* ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (head tm) 0)
      [➜ok]
      ))

;;--------------------------------------------------------------------------------
;; head stepping
;;
  (defun-typed s ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜rightmost (be ∅))
        &allow-other-keys
        )
      ➜
      (on-rightmost tm
        {
          :➜t ➜rightmost
          :➜∅ (λ()(incf (head tm))[➜ok])
          })))
                
  (defun-typed -s ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        (➜leftmost (be ∅))
        &allow-other-keys
        )
      ➜
      (on-leftmost tm
        {
          :➜t ➜leftmost
          :➜∅ (λ()(decf (head tm))[➜ok])
          })))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; 'a is not supported for a constant-length array, as the allocation
  ;; is made when the array is created, and can not be changed.
  ;; (defun-typed a ((tm constant-length-array-tm) instance &optional ➜)

;;--------------------------------------------------------------------------------
;; length-tape
;;
  (defun-typed tape-length-is-one ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if
        (= (max tm) 0)
        [➜t]
        [➜∅]
        )
      ))

  (defun-typed tape-length-is-two ((tm constant-length-array-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if
        (= (max tm) 1)
        [➜t]
        [➜∅]
        )))

      
