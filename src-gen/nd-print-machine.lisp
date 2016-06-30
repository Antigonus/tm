#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

This print facility was intended for use in debugging.

|#

(in-package #:tm)

   (defun indent (n) 
     (dotimes (i n)(princ "  "))
     )

   (defgeneric nd-print-machine (tm &optional n))
   (defmethod nd-print-machine ((tm nd-tape-machine) &optional (n 0))
     (indent n) (princ tm) (nl)
     (indent n) (princ "state: ") (princ (type-of (state tm)))(nl)
     (indent n) (princ "HA: ") (princ (HA tm)) (nl)
     (indent n) (princ "tape: ") (princ (tape tm)) (nl)
     (indent n) (princ "parameters: ") (princ (parameters tm)) (nl)
     )
   
