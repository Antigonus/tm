#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package #:tm)


;;--------------------------------------------------------------------------------
;; tm-si
;;
  (defun test-si-0 ()
    (let*(
           (y '(1 2 (3 4) 5))
           (ytm (mk-tm 'tm-list y))
           )
      (s ytm)
      (s ytm)
      (si ytm)
      (eql (r ytm) 3)
      ))
  (test-hook test-si-0)     

  (defun test-si-1 ()
    (let*(
           (y0 (mk-tm 'tm-list '(1 2 3)))
           (y1 (mk-tm 'tm-list `(11 12 ,y0 13)))
           )
      (∧
        (= (r y1) 11)
        (s y1)
        (= (r y1) 12)
        (s y1)
        (si y1)
        (= (r y1) 1)
        (s y1)
        (= (r y1) 2)
        (s y1)
        (= (r y1) 3)
        (¬ (s y1))
        )
      ))
  (test-hook test-si-1)     
