#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)
(defun test-tm-derived-0 ()
  (let*(
         (tm1 (mk 'list-tm {:tape {7 2 -3}}))
         )
    (∧
      (on-leftmost tm1)
      (= (r tm1) 7)
      (¬ (on-rightmost tm1))
      (c◨ tm1)
      (¬ (on-leftmost tm1))
      (on-rightmost tm1)
      (= (r tm1) -3)
      (s tm1 {:➜ok (be ∅) :➜rightmost (be t)})
      )))
(test-hook test-tm-derived-0)


(defun test-tm-derived-1 ()
  (let*(
         (tm1 (mk 'list-tm {:tape {7 2 -3}}))
         )
    (∧
      (= (r tm1) 7)
      (as tm1 21)
      (= (r tm1) 21)
      (s tm1)
      (= (r tm1) 2)
      (s tm1)
      (= (r tm1) -3)
      (on-rightmost tm1)
      (as tm1 31)
      (= (r tm1) 31)
      (on-rightmost tm1)
      (s tm1 {:➜ok (be ∅) :➜rightmost (be t)})
      )))
(test-hook test-tm-derived-1)


;; a&h◨ will come from the more specific list implementation, rather from
;; the generic implemenation.  I know of no direct way to force LISP to
;; ignore the more specific implemnation in favor of the the generic one.
(defun test-tm-derived-2 ()
  (let*(
         (tm1 (mk 'list-tm {:tape {7 2 -3}}))
         )
    (∧
      (c◨ tm1)
      (a&h◨ tm1 77)
      (= (r tm1) -3)
      (¬ (on-rightmost tm1))
      (s tm1)
      (on-rightmost tm1)
      (= (r tm1) 77)
      )))
(test-hook test-tm-derived-2)

;; as&h◨ will come from the more specific list implementation, rather from
;; the generic implemenation.  I know of no direct way to force LISP to
;; ignore the more specific implemnation in favor of the the generic one.
(defun test-tm-derived-3 ()
  (let*(
         (tm1 (mk 'list-tm {:tape {7 2 -3}}))
         )
    (∧
      (c◨ tm1)
      (as&h◨ tm1 77)
      (= (r tm1) 77)
      (on-rightmost tm1)
      )))
(test-hook test-tm-derived-3)
