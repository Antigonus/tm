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

   (defgeneric tm-print-0 (tm &optional n))
   (defmethod tm-print-0 ((tm tape-machine) &optional (n 0))
     (indent n) (princ tm) (nl)
     (indent n) (princ "head: ") (princ (head tm)) (nl)
     (indent n) (princ "tape: ") (princ (tape tm)) (nl)
     )


   (defun print-tm-instance-in-brackets (instance)
     (princ "[") (princ instance) (princ "]")
     )
   (defun print-tm-instance-in-parens (instance)
     (princ "(") (princ instance) (princ ")")
     )

   ;; later would be nice to check tape length, and add ellipses showing just
   ;; context at the beginning of the tape, around the head position, and at the
   ;; end of the tape
   (defgeneric tm-print (tm))
   (defmethod tm-print ((tm tape-machine))
     (let(
           (original-head (head tm)) ; big cheat!
           )
       (princ tm)
       (princ " ")
       (cue-leftmost tm)
       (⟳(λ(cont-loop cont-return)
           (s tm
             (λ()
               (if
                 (eq (head tm) original-head)
                 (print-tm-instance-in-brackets (r tm))
                 (print-tm-instance-in-parens (r tm))
                 )
               (funcall cont-loop)
               )
             (λ()
               (setf (head tm) original-head)
               (nl)
               (funcall cont-return)
               ))))))

