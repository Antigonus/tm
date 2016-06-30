#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

   (defun indent (n) 
     (dotimes (i n)(princ "  "))
     )


   (defun print-machine-0 (tm &optional (n 0))
     (indent n) (princ tm) (nl)
     (indent n) (princ "state: ") (princ (type-of (state tm)))(nl)
     (indent n) (princ "HA: ") (princ (HA tm)) (nl)
     (indent n) (princ "tape: ") (princ (tape tm)) (nl)
     (indent n) (princ "parameters: ") (princ (parameters tm)) (nl)
     )

   ;; does not recursively descend into entangled machines, just prints their ids
   (defun print-entanglements-0 (tm &optional (n 0))
     (indent n) (princ "entanglements:") (nl)
     (let(
           (es (entanglements tm))
           )
       (unless es 
         (indent (1+ n))
         (princ "∅")
         (nl)
         (return-from print-entanglements-0)
         )
       (cue-leftmost es
         (λ() ; all cued up
           (⟳(λ(cont-loop cont-return)
               (r es
                 (λ(entangled-tm) 
                   (indent (1+ n))
                   (if
                     (eq entangled-tm tm) 
                     (princ "self: ")
                     (progn (princ "[") (princ (r entangled-tm)) (princ "]: "))
                     )
                   (princ entangled-tm) 
                   (nl)
                   )
                 #'cant-happen ; we called cue-leftmost to get here
                 )
               (s es cont-loop cont-return)
               )))
         (λ() ; cue failed
           (indent n)(princ "#empty")(nl)
           ))))

   (defgeneric print-machine (tm &optional n))
   (defmethod print-machine (tm &optional (n 0))
     (print-machine-0 tm n)
     (print-entanglements-0 tm n)
     )

   
