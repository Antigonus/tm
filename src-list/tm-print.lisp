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
     (indent n) (princ "HA: ") (princ (HA tm)) (nl)
     (indent n) (princ "tape: ") (princ (tape tm)) (nl)
     )


   (defun print-tm-object-in-brackets (object)
     (princ "[") (princ object) (princ "]")
     )
   (defun print-tm-object-in-parens (object)
     (princ "(") (princ object) (princ ")")
     )

   ;; later would be nice to check tape length, and add ellipses showing just
   ;; context at the beginning of the tape, around the head position, and at the
   ;; end of the tape
   (defgeneric tm-print (tm))
   (defmethod tm-print ((tm tape-machine))
     (let(
           (original-HA (HA tm)) ; big cheat!
           )
       (princ tm)
       (princ " ")
       (cue-leftmost tm)
       (⟳(λ(cont-loop cont-return)
           (s tm
             (λ()
               (if
                 (eq (HA tm) original-HA)
                 (print-tm-object-in-brackets (r tm))
                 (print-tm-object-in-parens (r tm))
                 )
               (funcall cont-loop)
               )
             (λ()
               (setf (HA tm) original-HA)
               (nl)
               (funcall cont-return)
               ))))))

