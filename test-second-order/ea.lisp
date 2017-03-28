#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)


(defun test-ea-0 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {1 2 3}}))
         (tm1 (mk 'ea-tm {:base tm0}))
         (tm2 (entangle tm1))
         )
    (∧
      (typep tm1 'status-active)
      (typep tm2 'status-active)
      (= (r tm1) 1)
      (= (r tm2) 1)
      (= (address tm1) 0)
      (= (address tm2) 0)
      (= (address-rightmost tm1) 2)
      (= (address-rightmost tm2) 2)
      (= (d tm1) 2)
      (= (address-rightmost tm1) 1)
      (= (address-rightmost tm2) 1)
      (s tm2)
      (= (r tm2) 3)
      (= (address tm2) 1)
      (clean-entanglements tm1)
      )))
(test-hook test-ea-0)

(defun test-ea-1 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {1 2 3}}))
         (tm1 (mk 'ea-tm {:base tm0}))
         )
    (∧
      (park tm1)
      (= (d◧ tm1) 1)
      (= (d◧ tm1) 2)
      (= (d◧ tm1) 3)
      (typep tm1 'status-empty)
      )
    ))
(test-hook test-ea-1)

(defun test-ea-2 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {1 2 3}}))
         (tm1 (mk 'ea-tm {:base tm0}))
         (tm2 (entangle tm1))
         )
    (∧
      (park tm1)
      ;; this has a collision with tm2's head
      ;;
         (= (d◧ tm1 ∅ {:➜ok #'echo :➜collision (be 7)}) 7)
      (park tm2)
      (= (d◧ tm1) 1)
      (= (d◧ tm1) 2)
      (= (d◧ tm1) 3)
      (typep tm2 'status-empty)
      )))
(test-hook test-ea-2)

(defun test-ea-3 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {1 2 3}}))
         (tm1 (mk 'ea-tm {:base tm0}))
         )
    (∧
      (typep tm0 'tape-machine)
      (typep tm1 'ea-tm)
      (typep tm1 'status-tm)
      (park tm1)
      (typep tm1 'ea-parked)
      (typep tm1 'status-parked)
      (d tm1)
      (d tm1)
      (d tm1)
      (typep tm1 'ea-empty)
      (typep tm1 'status-empty)
      )))
(test-hook test-ea-3)

(defun test-ea-4 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {∅}}))
         (tm1 (mk 'ea-tm {:base tm0 :empty t}))
         )
    (∧
      (a◧ tm1 10)
      (a◧ tm1 20)
      (a◧ tm1 30)
      (typep tm1 'status-parked)
      (s tm1)
      (= (r tm1) 30)
      (s tm1)
      (= (r tm1) 20)
      (s tm1)
      (= (r tm1) 10)
      (¬ (s tm1))
      )
    ))
(test-hook test-ea-4)
