#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package :tm)

(defstruct counter-box-struct
  (counter 0)
  )

(def-worker count-three counter-box () tm-dst 
  (&optional 
    (cont-ok (be 'ok)) 
    (cont-bound (be 'bound))
    (cont-no-alloc (be 'no-alloc))
    )
  (if
    (≥ (counter-box-struct-counter counter-box)  3)
    (funcall cont-bound)
    (progn
      (as tm-dst (counter-box-struct-counter counter-box)
        (λ() 
          (incf (counter-box-struct-counter counter-box))
          (funcall cont-ok)
          )
        cont-no-alloc
        ))
    ))

(defun test-count-three-0 ()
  (let(
        (tm (mk-tm-list))
        (counter-box (make-counter-box-struct))
        )
    (∧
      (count-three counter-box tm)
      (count-three counter-box tm)
      (count-three counter-box tm)
      (eq (count-three counter-box tm) 'bound)
      (equal (tape tm) '(list 0 1 2))
      )))
(test-hook test-count-three-0)    
  
