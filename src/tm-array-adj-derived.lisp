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
;; accessing data
;;
  (defmethod r-index
    (
      (tm tm-array-adj)
      index
      &optional 
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
        (funcall cont-ok (aref (tape tm) read-index ))
        )))

;;--------------------------------------------------------------------------------
;; absolute head placement
;;
  (defmethod cue-rightmost ((tm tm-array-adj)) (setf (HA tm) (rightmost-index tm)))

;;--------------------------------------------------------------------------------
;;  tape-machine properties
;;
  (defmethod on-rightmost
    (
      (tm0 tm-array-adj)
      &optional
      (cont-true (be t))
      (cont-false (be ∅))
      )
    (if
      (= (HA tm0) (rightmost-index tm0))
      (funcall cont-true)
      (funcall cont-false)
      ))


