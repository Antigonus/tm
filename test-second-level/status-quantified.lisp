#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-status-filter-0 ()
  (let*(
         (tm0 (mk 'status-tm {:base-type 'list-haz-tm :tape {1 2 3 4 5} :status 'parked}))
         (tm1 (mk 'status-tm {:base-type 'list-haz-tm :status 'empty}))
         (pred (λ(tm ct c∅) (declare (ignore tm c∅)) [ct]))
         )
    (∧
      (filter tm0 tm1 pred)
      (typep tm0 'empty)
      (equal (tape (base tm1)) {1 2 3 4 5})
      (on-rightmost tm1)
      )))
(test-hook test-status-filter-0)

(defun test-status-filter-1 ()
  (let*(
         (tm0 (mk 'status-tm {:base-type 'list-haz-tm :tape {1 2 3 4 5} :status 'parked}))
         (tm1 (mk 'status-tm {:base-type 'list-tm :status 'empty}))
         (pred (λ(tm ct c∅)(if (oddp (esr tm)) [c∅] [ct]))) ;; need 'esr' here !
         )
    (∧
      (filter tm0 tm1 pred)
      (equal (tape (base tm0)) {1 3 5})
      (equal (tape (base tm1)) {2 4})
      (on-rightmost tm1)
      )))
(test-hook test-status-filter-1)

