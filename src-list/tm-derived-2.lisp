#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

These functions derive the remainder of the tape-machine interface while using only the
primitives from tm-primitives.  

There is no functional need for a new tape machine implementation to specialize these
functions.  Still, some implementations will want to specialize these functions for
performance reasons.

Because these are built upon the primitives, they can only be tested against implementations
of the primitives.


|#
(in-package #:tm)

;;--------------------------------------------------------------------------------
;; tape-machine duplication
;;   we need a layer 0 with no entanglement accounting in order to implement the
;;   entanglement list functions sans circular references.
;;
  ;; cue-to-0 is primitive

  ;; adds entanglement accounting to cue-to-0 result
  (defun cue-to-2
    (
      tm-cued 
      tm-orig
      )
    (cue-to-0 tm-cued tm-orig)
    (let(
          (es (entanglements tm-orig))
          )
      (setf (entanglements tm-cued) es)
      (when es (a (entanglements tm-cued) tm-cued #'do-nothing #'cant-happen))
      )
    tm-cued
    )

  ;; this works when the head is a value, such as an integer or cons.  However, if it is a
  ;; reference, then a deeper copy will be needed. Note for example, tm-region
  (defun cue-to (tm-cued tm-orig)
    "tm-cued machine will be rewritten.  It will be change-class'ed to the same type as
     tm-orig, it will share the same tape, entanglesments, and parameters as tm-orig,
     though have an indendent head.  The head is initially on the same cell as that of
     tm-orig.  tm-cued is added to the entanglement list.
     "
    (disentangle tm-cued) ; the entangled machines will no longer see tm-cued
    (change-class tm-cued (type-of tm-orig))
    (cue-to-2 tm-cued tm-orig)
    tm-cued
    )

  (defun dup (tm-orig)
    "Returns a new tm cued to tm-orig."
    (let(
          (tm-cued (make-instance (type-of tm-orig)))
          )
      (cue-to-2 tm-cued tm-orig)
      tm-cued
      ))

  ;; Mounts the same tape that another machine has mounted.
  ;; Unlike dup, upon exit the head is at leftmost.
  (defmethod mount ((tm tape-machine) &optional (cont-ok #'echo) cont-fail)
    (declare (ignore cont-fail))
    (let(
          (tm-dup (dup tm))
          )
      (cue-leftmost tm-dup)
      (funcall cont-ok tm-dup)
      ))

