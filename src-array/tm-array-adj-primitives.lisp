#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  An adjustable array facilitates new allocation on rightmost.  In contrast, a fixed array
  allocation is more complicated.  Rightmost can be extended only until righmost hits the 
  upperbound.

  If we were to emulate allocation, #a, in the middle of the array by moving data, we run
  into the probem that multiple tms sharing a tape all need to have their heads
  adjusted ;-) However, allocating from rightmost is safe as other machine heads can't be
  located beyond rightmost.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;;  helpers
;;
  ;; for local use
  ;; we use inclusive bounds, Lisp provides an exclusive bound
  (defun rightmost-index (tm) (1- (length (tape tm))))

;;--------------------------------------------------------------------------------
;; accessing data
;;
  (defmethod r ((tm tm-array-adj)) (aref (tape tm) (HA tm) ))
  (defmethod w ((tm tm-array-adj) object)
    (setf (aref (tape tm) (HA tm)) object)
    t
    )

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defmethod cue-leftmost  ((tm tm-array-adj)) 
    (setf (HA tm) 0)
    )

;;--------------------------------------------------------------------------------
;;  head location predicates
;;
  (defmethod heads-on-same-cell 
    (
      (tm0 tm-array-adj) 
      (tm1 tm-array-adj) 
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
;; head stepping
;;
  (defmethod s
    (
      (tm tm-array-adj)
      &optional
      (cont-ok (be t))
      (cont-rightmost (be ∅))
      )
    (if
       (< (HA tm) (rightmost-index tm))
       (progn
         (incf (HA tm))
         (funcall cont-ok)
         )
      (funcall cont-rightmost)
      ))


;;--------------------------------------------------------------------------------
;; cell allocation
;;
  ;; allocates a cell just to the right of the head an initializes it with object
  (defmethod a 
    (
      (tm tm-array-adj)
      object 
      &optional
      (cont-ok (be t))
      (cont-no-alloc (λ()(error 'tm-alloc-fail :text "alloc called in the middle of an array")))
      )
    (if
      (= (HA tm) (rightmost-index tm))
      (progn
        (vector-push-extend object (tape tm))
        (funcall cont-ok)
        )
      (funcall cont-no-alloc)
      ))
        
;;--------------------------------------------------------------------------------
;; deallocating cells
;;
  ;; deallocates the cell just to the right of the head
  (defmethod d 
    (
      (tm tm-array-adj)
      &optional 
      spill
      (cont-ok #'echo)
      (cont-rightmost (λ()(error 'tm-deallocation-request-at-rightmost)))
      (cont-no-alloc (λ()(error 'tm-alloc-fail :text "alloc called in the middle of an array")))
      )
    (cond
      ((= (HA tm) (rightmost-index tm)) (funcall cont-rightmost))
      ((= (HA tm) (1- (rightmost-index tm)))
        (let(
              (displaced-data (vector-pop (tape tm)))
              )
          (a spill displaced-data cont-ok cont-no-alloc)
          ))
      (t
        (funcall cont-no-alloc)
        )))
          

    
