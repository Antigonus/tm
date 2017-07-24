#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-tape-0 ()
  (let*(
        (c3 (mk 'cell-list 43 {:status 'right-bound}))
        (c2 (mk 'cell-list 32 {:status 'interior   :right-neighbor c3}))
        (c1 (mk 'cell-list 21 {:status 'left-bound :right-neighbor c2}))
        (c0 (mk 'cell-list 10))
       )
    (∧
      (epa<tape> c1 c0)
      (= (◧r c0 {:n 0}) 10)
      (= (◧r c0 {:n 1}) 21)
      (= (◧r c0 {:n 2}) 32)
      (= (◧r c0 {:n 3}) 43)
      (= 
        (◧r c0 {:n 4 
                   :➜ok (be 5)
                   :right-bound (λ(cell n)(declare (ignore cell)) n)
                   })
        1
        )
      )))    
(test-hook test-tape-0)

