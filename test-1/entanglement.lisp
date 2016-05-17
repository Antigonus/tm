#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(defun test-collision-0 ()
  (let*(
         (tm0 (mount {1 2 3}))
         (tm1 (dup tm0))
         (tm2 (dup tm0))
         )
    (s tm0)
    (s tm1)
    (∧
      (collision tm0 tm1)
      (collision tm1 tm0)
      (¬ (collision tm0 tm2))
      (¬ (collision tm2 tm0))
      (¬ (collision tm1 tm2))
      (¬ (collision tm2 tm1))
      )))

(defun test-∃-collision-0 ()
  (let((tm0 (mount (loop for i from 1 to 10 collecting i))))
    (let((tm1 (dup tm0)))
      (sn tm0 5)
      (let((tm2 (dup tm0)))
        (∧
          (¬ (∃-collision tm1))
          (∃-collision tm2)
          )))))


