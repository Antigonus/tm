#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package :tm)

(defstruct state count sum)

(defun sum (tm a-state)
  (incf (state-count a-state))
  (setf (state-sum a-state) (+ (state-sum a-state) (r tm)))
  (state-sum a-state)
  )

(defun tm-transform-test-0 ()
  (let*(
         (tm0 (mount {1 2 3}))
         (state-0 (make-state :count 0 :sum 0))
         (tm1 (tm-mk 'tm-transform tm0 state-0  #'sum))
         )
    (∧
      (= 1 (r tm0))
      (= 1 (r tm1))
      (w tm1 7)
      (s tm0)
      (s tm1)
      (= 2 (r tm0))
      (= 3 (r tm1))
      (w tm1 8)
      (s tm0)
      (s tm1)
      (= 3 (r tm0))
      (= 6 (r tm1))
      (w tm1 9)
      (equal (unmount tm0) [7 8 9])
      )
    ))
(test-hook tm-transform-test-0)
