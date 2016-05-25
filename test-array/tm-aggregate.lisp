#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-tm-aggregate-0 ()
  (let*(
         (tm0 (mount {1}))
         (tm1 (mount {2 3}))
         (tm2 (mount {4}))
         (tm3 (tm-mk 'tm-aggregate tm0 tm1 tm2))
         (tm4 (tm-mk 'tm-smooth tm3))
         )
    (∧
      (= (r tm4) 1)
      (s tm4)
      (= (r tm4) 2)
      (s tm4)
      (= (r tm4) 3)
      (s tm4)
      (= (r tm4) 4)
      (¬ (s tm4))
      )))
(test-hook test-tm-aggregate-0)
