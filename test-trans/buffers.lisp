#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-buffers-0 ()
  (let*(
         (location (mount [0 1 2 3 4 5 6 7]))
         (rightmost (fork location))
         )
    (sn location 2)
    (sn rightmost 5)
    (let(
          (tmr (mk 'tm-region :location location :rightmost rightmost))
          )
      (let(
            (q0  (make-instance 'queue :buffer tmr))
            )
      (∧
        (equal (to-list tmr) [3 4 5])
        (enqueue q0 'a)
        (enqueue q0 'b)
        (enqueue q0 'c)
        (= (dequeue q0) 3)
        (= (dequeue q0) 4)
        (equal (to-list tmr) [5 a b c])
        )
        ))))
(test-hook test-buffers-0)

