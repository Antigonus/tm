#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-list-tm-0 ()
  (let*(
         (tm0 (make-instance 'list-tm))
         (i2 (make-instance 'list-tm))
         (tm1 (mk 'list-tm :mount {-1 2 3}))
         )
    (∧
      (init tm0 {:mount {1}})
      (init i2 {} (be ∅) (be t)) ; i2 init fails for lack of inialization data
      (init i2 {:mount {}} (be ∅) (be t)) ; i2 init fails for lack of inialization data

      (typep tm0 'list-tm)
      (typep tm1 'list-tm)

      (equal (tape tm0) {1})
      (equal (HA tm0) {1})

      (equal (tape tm1) {-1 2 3})
      (equal (HA tm1) {-1 2 3})

      (eq (tape tm0) (HA tm0))
      (eq (tape tm1) (HA tm1))
      )))
(test-hook test-list-tm-0)


(defun test-list-tm-1 ()
  (let*(
         (tm0 (mk 'list-tm :mount {1}))
         (tm1 (mk 'list-tm :mount {-1 2 3}))
         )
    (∧
      (= (r tm0) 1)
      (= (r tm1) -1)

      (s tm0 (be ∅) (be t))
      (s tm1)

      (= (r tm0) 1)
      (= (r tm1) 2)

      (w tm0 -2)
      (= (r tm0) -2)

      (a tm0 10)
      (a tm1 11)

      (= (r tm0) -2)
      (= (r tm1) 2)

      (equal (tape tm0) {-2 10})
      (equal (tape tm1) {-1 2 11 3})

      (s tm0)
      (s tm1)

      (= (r tm0) 10)
      (= (r tm1) 11)

      (s tm1)

      (= (r tm1) 3)

      (equal (tape tm0) {-2 10})
      (equal (tape tm1) {-1 2 11 3})
      
      (s tm0 (be ∅) (be t))
      (s tm1 (be ∅) (be t))
      )
    ))
(test-hook test-list-tm-1)

(defun test-list-tm-2 ()
  (let*(
         (tm0 (mk 'list-tm :mount {1}))
         (tm1 (mk 'list-tm :mount {-1 2 3}))
         )
    (∧
      (cue-rightmost tm0)
      (cue-rightmost tm1)

      (= (r tm0) 1)
      (= (r tm1) 3)

      (cue-leftmost tm0)
      (cue-leftmost tm1)

      (= (r tm0) 1)
      (= (r tm1) -1)

      (as tm1 11)

      (= (r tm1) 11)
      (equal (tape tm1) {-1 11 2 3})

      (a&h◨ tm0 21)
      (= (r tm0) 1)
      (equal (tape tm0) {1 21})
      
      (s tm0)
      (as&h◨ tm0 32)
      (= (r tm0) 32)
      (equal (tape tm0) {1 21 32})

      (s tm0 (be ∅) (be t))

      (= (r tm1) 11)
      (s tm1)
      (= (r tm1) 2)
      (s tm1)
      (= (r tm1) 3)
      (s tm1 (be ∅) (be t))
      (= (r tm1) 3)

      )))
(test-hook test-list-tm-2)
      
