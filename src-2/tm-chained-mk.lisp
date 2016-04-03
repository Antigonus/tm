#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Array blocks are chained together.  Stepping seamlessly moves
between the blocks. 

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
;; tm-chained is 
;;  tape holds another tape machine, that other tape machine is a list of blocks
;;
  (defclass tm-chained (tape-machine)())

;;--------------------------------------------------------------------------------
;;
  (defmethod tm-init
    (
      (i tm-chained)
      &optional 
      init
      (cont-ok #'echo) 
      (cont-fail 
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized chained tape type") ∅)
        ))
    (cond

      ;; must intiialize against the first block
      ((∨ (¬ init) (typep init 'tm-chained)) (funcall cont-fail)) 

      ((typep init 'array)
        (let*(
               (a0 (tape init))
               (n (length a0))
               (a1 (make-chained n))
               )
          (setf (HA i) 0)
          (setf (tape i) a1)
          (loop for k from 1 to (1- n) do
            (setf (aref a1 k) (aref a0 k)) ; look at all that waste, wish I had a pointer
            (funcall cont-ok i)
            )))

      ((typep init 'sequence)
        (let*(
               (tm0 (mount init))
               (n (length init))
               (a1 (make-chained n))
               (k 0)
               )
          (setf (HA i) 0)
          (setf (tape i) a1)
          (⟳ (λ(cont-loop cont-return)
               (setf (aref a1 k) (r tm0))
               (incf k)
               (s tm0 cont-loop cont-return)
               ))
          (funcall cont-ok i)
          ))

      (t
        (funcall cont-fail)
        )))

  ;;--------------------------------------------------------------------------------
  ;;  there is no type of fundamental sequence which chained will mount




 
  
