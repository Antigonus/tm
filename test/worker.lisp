#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package :tm)

(defstruct counter-box-struct
  (counter 0)
  )

(def-worker count-two counter-box () tm-dst 
  (&optional 
    (cont-ok (be 'ok)) 
    (cont-bound (be 'bound))
    (cont-no-alloc (be 'no-alloc))
    )
  (progn
    (as tm-dst (counter-box-struct-counter counter-box)
      (λ() 
        (if
          (≥ (counter-box-struct-counter counter-box) 2)
          (funcall cont-bound)
          (progn
            (incf (counter-box-struct-counter counter-box))
            (funcall cont-ok)
            )))
      cont-no-alloc
      )))

(defun test-count-two-0 ()
  (let(
        (tm (mk-tm-list))
        (counter-box (make-counter-box-struct))
        )
    (∧
      (eq (count-two counter-box tm) 'ok)
      (eq (count-two counter-box tm) 'ok)
      (eq (count-two counter-box tm) 'bound)
      (equal (tape tm) '(list 0 1 2))
      )))
(test-hook test-count-two-0)    
  
