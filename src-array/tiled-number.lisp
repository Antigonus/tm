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

(defparameter *tile-length* 4) ; 4 bit tiles, 16 bytes addressed = two 64 bit words

(defun shift-left (x n)
  (floor (* x (expt 2 (- n))))
  )

(defun prinx (x) (format nil "~x"x))

;; this tm walks the tiles in an address
(def-type tiled-number-tm (tape-machine)
  (
    (head ; current bit location
      :initarg :head 
      :accessor head
      )
    (tape ; numeric-address
      :initarg :tape
      :accessor tape
      )))

;;--------------------------------------------------------------------------------
;;  Make tiled-number machines. Typically the tm will be a array-nd-tm or a array-solo-tm
;;  instance, and then this gets called due to the inheritance structure.
;;
  (defun-typed init 
    (
      (tm tiled-number-tm)
      &optional 
      keyed-parms
      ➜
      )
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys          
        )
      ➜
      (destructuring-bind
        (&key tape) keyed-parms
        (setf (head tm) 0)
        (cond
          ((∧ tape (numberp tape))
            (setf (tape tm) tape)
            [➜ok tm]
            )
          (t
            (setf (tape tm) 0)
            (call-next-method keyed-parms ➜) ; pick up tape-machine's init for non consp tapes
            ))
        )))

;;--------------------------------------------------------------------------------
;; location
;;  
  ;; We should always be aligned on a tile, so being at position 0 is leftmost.
  ;; However, so that a tile alignment bug doesn't cause an infinite loop I've 
  ;; made the test a bit defensive.
  (defun-typed on-leftmost ((tm tiled-number-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if (< (head tm) *tile-length*) [➜t] [➜∅])
      ))

  (defun-typed on-rightmost ((tm tiled-number-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if 
        (= 0 (shift-left (tape tm) (+ (head tm) *tile-length*)))
        [➜t]
        [➜∅]
      )))

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defun-typed r ((tm tiled-number-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (ldb (byte *tile-length* (head tm)) (tape tm))]
      ))

  (defun-typed esr ((tm tiled-number-tm) &optional ➜)
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
                [➜ok  (ldb (byte *tile-length* (+ (head tm) *tile-length*)) (tape tm))]
                )
          })))

  (defun-typed w ((tm tiled-number-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (tape tm) (dpb instance (byte *tile-length* (head tm)) (tape tm)))
      [➜ok]
      ))

  (defun-typed esw ((tm tiled-number-tm) instance &optional ➜)
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
                (setf
                  (tape tm)
                  (dpb instance (byte *tile-length* (+ (head tm) *tile-length*)) (tape tm))
                  )
                [➜ok]
                )
          })))

  (defun-typed e◧r ((tm tiled-number-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        )
      ➜
      [➜ok (ldb (byte *tile-length* 0) (tape tm))]
      ))

  (defun-typed e◧sr ((tm tiled-number-tm) &optional ➜)
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
                [➜ok (ldb (byte (+ *tile-length* *tile-length*) *tile-length*) (tape tm))]
                )
          })))

  (defun-typed e◧w ((tm tiled-number-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (tape tm) (dpb instance (byte *tile-length* 0) (tape tm)))
      [➜ok]
      ))

  (defun-typed e◧sw ((tm tiled-number-tm) instance &optional ➜)
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
          :➜∅ (λ()
                (setf 
                  (tape tm)
                  (dpb instance (byte (+ (* 2 *tile-length*)) *tile-length*) (tape tm)))
                [➜ok]
                )
          })))


;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defun-typed ◧ ((tm tiled-number-tm) &optional ➜)
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
  (defun-typed s ((tm tiled-number-tm) &optional ➜)
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
          :➜∅ (λ()(incf (head tm) *tile-length*)[➜ok])
          })))
                
  (defun-typed -s ((tm tiled-number-tm) &optional ➜)
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
          :➜∅ (λ()(decf (head tm) *tile-length*)[➜ok])
          })))

;;--------------------------------------------------------------------------------
;; cell allocation
;;
  (defun-typed a ((tm tiled-number-tm) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (let*(
             (the-number (tape tm))
             (address-current-tile (head tm))
             (address-next-tile (+ address-current-tile *tile-length*))
             )
        (let*(
               (left-hand-side (ldb (byte address-next-tile 0) the-number))
               (right-hand-side (- the-number left-hand-side))
               (number-with-hole  (+ left-hand-side (* (expt 2 *tile-length*) right-hand-side)))
             )
        (setf
          (tape tm)
          (dpb
            instance
            (byte *tile-length* address-next-tile)
            number-with-hole
            ))
          [➜ok]
          ))))

;;--------------------------------------------------------------------------------
;; length-tape
;;
  (defun-typed tape-length-is-one ((tm tiled-number-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if
        (∧
          (≠ 0 (tape tm))
          (= 0 (floor (* (expt 2 (- *tile-length*)) (tape tm))))
          )
        [➜t]
        [➜∅]
        )
      ))

  (defun-typed tape-length-is-two ((tm tiled-number-tm) &optional ➜)
    (destructuring-bind
      (&key
        (➜t (be t))
        (➜∅ (be ∅))
        &allow-other-keys
        )
      ➜
      (if
        (∧
          (≠ 0 (floor (* (expt 2 (- *tile-length*)) (tape tm))))
          (= 0 (floor (* (expt 2 (- (* 2 *tile-length*))) (tape tm))))
          )
        [➜t]
        [➜∅]
        )))

      
