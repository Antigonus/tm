#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package #:tm)

;;--------------------------------------------------------------------------------
;; absolute location
;;
  (defun test-on+1-0 ()
    (let(
          (k (mount {1 2 3}))
          )
      (s k)
      (on+1 k)
      ))
  (test-hook test-on+1-0)
                         
  (defun test-location-0 ()
    (let(
          (k (mount {1 2 3 4 5}))
          )
      (∧
        (= (address k) 0)
        (on+n k 0)
        (on-rightmost+n k -4)

        (sn k 3)

        (= (address k) 3)
        (on+n k 3)
        (on-rightmost-1 k)
        (on-rightmost+n k -1)

        (s k)

        (= (address k) 4)
        (on+n k 4)
        (on-rightmost k)
        (on-rightmost+n k 0)

        (on+n k 5 (be ∅) (be t))
        )))
    (test-hook test-location-0)

;;--------------------------------------------------------------------------------
;; relative location
;;
  (defun test-distance-1 ()
    (let*(
           (k0 (mount {1 2 3 4 5}))
           (k1 (dup k0))
           )
      (∧
        (distance+n k0 k1 0)
        (distance+1 k0 k1 (be ∅) (be t))
        (s k1)
        (distance+1 k0 k1)
        (distance+n k0 k1 1)
        (distance+n k0 k1 2 (be ∅) (be t))
        (s k1)
        (distance+1 k0 k1 (be ∅) (be t))
        (distance+n k0 k1 1 (be ∅) (be t))
        (distance+n k0 k1 2)
        (distance+n k0 k1 3 (be ∅) (be t))
        )))
   (test-hook test-distance-1)

  
  (defun test-location-cmp-0 ()
    (let*(
           (k0 (mount {1 2 3 4 5}))
           (k1 (dup k0))
           )
      (∧
        (location-cmp k0 0 (be ∅) (be t) (be ∅))
        (location-cmp k0 k1 (be ∅) (be t) (be ∅))
        (location≤ k0 k1)
        (location= k0 k1)
        (location≥ k0 k1)
        (location-cmp k0 1 (be t) (be ∅) (be ∅))
        (location< k0 1)
        (location≤ k0 1)
        (= (distance k0 k1) 0)

        (s k1)
        (= (distance k0 k1) 1)
        (location-cmp k1 0 (be ∅) (be ∅) (be t))
        (location> k1 0)
        (location> k1 k0)
        (location< k0 k1)
        (location≥ k1 k0)
        (location-cmp k1 1 (be ∅) (be t) (be ∅))
        (location= k1 1)

        (s k1)
        (s k0)
        (= (distance k0 k1) 1)
        (location-cmp k1 1 (be ∅) (be ∅) (be t))
        (location> k1 1)
        (location> k1 k0)
        (location< k0 k1)
        (location≥ k1 k0)
        (location-cmp k1 2 (be ∅) (be t) (be ∅))
        (location= k1 2)
        (location-cmp k1 3 (be t) (be ∅) (be ∅))
        (location< k1 3)

        (s k0)
        (location= k0 k1)
        (s k0)
        (location> k0 k1)
        )))
  (test-hook test-location-cmp-0)
             
