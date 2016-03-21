#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  An adjustable array facilitates new allocation on rightmost.  In contrast, a fixed array
  allocation is more complicated.  Rightmost can be extended only until righmost hits the 
  upperbound.

  If we were to emulate allocation by moving data, we run into the probem that multiple
  tms sharing a tape all need to have their heads adjusted ;-) However, allocating from
  rightmost is safe as other machine heads can't be located beyond rightmost.

  Moving data may well affect other machines, but those other machines still have
  correct head addresses, as head address cells, not data.

  note (adjustable-array-p a) for checking if an array is adjustable
  this may be needed as (type-of an-array) just returns (vector T size)
  both are also (typep a 'array).

  (array dimension d 0) returns allocation length for 0 dimension of array d, while
  (length d) returns fill pointer for vector d, as does (fill-pointer d).

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-array (tape-machine)
    (
      (HA 
        :initform 0
        )
      (tape
        :initform (make-array 0 :fill-pointer 0 :adjustable t)
        )
      ))

  ;; for local use
  (defun rightmost-index (tm) (1- (length (tape tm))))

  ;; This is used internally, it is forward reference friendly.
  ;; For the externally visible version, see tape-machine-mk.lisp
  (defun tm-mk-array-0
    (
      &optional 
      (init-value ∅) 
      (cont-ok #'echo) 
      (cont-fail 
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized array tape type") ∅)
        ))
    (cond
      ((¬ init-value) 
        (let(
              (i (make-instance 'tm-array))
              )
          (vector-push-extend 'array (tape i))
          (funcall cont-ok i)
          ))
      ((typep init-value 'array)
        (funcall cont-ok
          (make-instance 'tm-array :tape init-value)
          ))
      ((typep init-value 'tm-array)
        (funcall cont-ok
          (make-instance 'tm-array
            :HA (HA init-value)
            :tape (tape init-value)
          )))
      (t
        (funcall cont-fail)
        )))

;;--------------------------------------------------------------------------------
;;  tape-machine properties
;;

  (defmethod on-rightmost
    (
      (tm0 tm-array)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (if
      (= (HA tm0) (rightmost-index tm0))
      (funcall cont-true)
      (funcall cont-false)
      ))

  (defmethod heads-on-same-cell 
    (
      (tm0 tm-array) 
      (tm1 tm-array) 
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      ) 
    (if
      (= (HA tm0) (HA tm1))
      (funcall cont-true)
      (funcall cont-false)
      ))

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r ((tm tm-array)) (elt (tape tm) (HA tm) ))
  (defmethod w ((tm tm-array) object)
    (setf (elt (tape tm) (HA tm)) object)
    t
    )

  (defun test-tm-mk-array-0-0 ()
    (let*(
          (tm0 (tm-mk-array-0))
          (tm1 (tm-mk-array-0 #(7 2 -3)))
          (tm2 (tm-mk-array-0 tm1))
          )
      (and
        (eq (r tm0) 'array)
        (eql (r tm1) 7)
        (heads-on-same-cell tm1 tm2)
        )))
  (test-hook test-tm-mk-array-0-0)

  (defmethod r-index
    (
      (tm tm-array)
      &optional 
      (index 1)
      (cont-ok #'echo) 
      (cont-index-beyond-rightmost
        (λ() (error 'tm-read-beyond-rightmost :text "attempt to read beyond the rightmost cell of the array") ∅)
        )
      )
    (let(
          (read-index (+ index (HA tm)))
          )
      (if
        (> read-index (rightmost-index tm))
        (funcall cont-index-beyond-rightmost)
        (funcall cont-ok (elt (tape tm) read-index ))
        )))


;;--------------------------------------------------------------------------------
;; cueing
;;
  (defmethod cue-leftmost  ((tm tm-array)) (setf (HA tm) 0))
  (defmethod cue-rightmost ((tm tm-array)) (setf (HA tm) (rightmost-index tm)))

  (defun test-cue-array-0 ()
    (let(
          (x (tm-mk-array-0 #(a b c)))
          (y (tm-mk-array-0))
          )
      (and
        (eq (r x) 'a)
        (eq (r y) 'array)
        (s x)
        (cue-to y x)
        (eq (r x) 'b)
        (eq (r y) 'b)
        (heads-on-same-cell x y)
        (cue-rightmost x)
        (eq (r x) 'c)
        )))
  (test-hook test-cue-array-0)

;;--------------------------------------------------------------------------------
;; stepping
;;
  (defmacro s-work-array (tm cont-ok cont-rightmost)
    `(if
       (< (HA ,tm) (rightmost-index ,tm))
       (progn
         (incf (HA ,tm))
         (funcall ,cont-ok)
         )
       (funcall ,cont-rightmost)
       ))

  (defmethod s
    (
      (tm tm-array)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (s-work-array tm cont-ok cont-rightmost)
    )

  ;; this does not get specialized by the tree methods, thus makes
  ;; array stepping available to those implementations. An array step
  ;; over an array object is step over a subtree.
  (defmethod so
    (
      (tm tm-array)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (s-work-array tm cont-ok cont-rightmost)
    )

  (defun test-s-array-0 ()
    (let*(
           (y #(1 2 (3 4) 5))
           (ytm (tm-mk-array-0 y))
          )
      (and
        (s ytm)
        (s ytm)
        (equal '(3 4) (r ytm))
        (s ytm)
        (not (s ytm))
        )))
  (test-hook test-s-array-0) 


  ; step left one -- you wish ;-)
  ;;  

