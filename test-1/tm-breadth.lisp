#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  The machine's tape has been weaved through a tree.

|#

(in-package #:tm)


(defun test-tm-breadth-s-1 ()
  (let*(
         (a-tree '(1 (2 (3 4)) 5))
         (tm (tm-mk 'tm-breadth (mount a-tree)))
         )
    (and
      (= (r tm) 1)

      (s tm)
      (equal (r tm) '(2 (3 4)))

      (s tm)
      (= (r tm) 5)

      (s tm)
      (= (r tm) 2)

      (s tm)
      (equal (r tm) '(3 4))

      (s tm)
      (= (r tm) 3)

      (s tm)
      (= (r tm) 4)

      (not (s tm))
      )))
(test-hook test-tm-breadth-s-1)

