#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-array (tape-machine)())

;;--------------------------------------------------------------------------------
;;
  (defmethod tm-init
    (
      (i tm-array)
      &optional 
      init
      (cont-ok #'echo) 
      (cont-fail 
        (λ() (error 'tm-mk-bad-init-type :text "unrecognized array tape type") ∅)
        ))
    (cond

      ((¬ init) (funcall cont-fail)) ; must be initialied to a fixed array

      ((typep init 'tm-array)
        (let*(
               (a0 (tape init))
               (n (length a0))
               (a1 (make-array n))
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
               (a1 (make-array n))
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
  ;; see the mount for tm-array-adj-mk as it also mounts the fixed array



 
  
