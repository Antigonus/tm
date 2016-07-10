#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

   ;; does not recursively descend into entangled machines, just prints their ids
   (defun print-entanglements (tm &optional (n 0))
     (indent n) (princ "entanglements:") (nl)
     (let(
           (es (entanglements tm))
           )
       (unless es 
         (indent (1+ n))
         (princ "∅")
         (nl)
         (return-from print-entanglements)
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
     (nd-print-machine tm n)
     (print-entanglements tm n)
     )

   
