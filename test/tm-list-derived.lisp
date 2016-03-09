#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(defun test-as-0 ()
  (let*(
         (tm0 (mk-tm-list (list 7 9 11)))
         (tm1 (mk-tm-list tm0))
         )
    (as tm0 8)
    (s tm0)
    (as tm0 10)
    (and
      (= (r tm0) 10)
      (equal (HA tm1) '(7 8 9 10 11)) ; head is not for public use
      )))
