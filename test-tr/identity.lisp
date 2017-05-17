#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-identity-0 ()
  (let*(
        (tm0 (mk 'list-solo-tm {:tape {1 2 3}}))
        (tm1 (mk 'identity-tr {:base tm0}))
        )
    (epd tm1 ∅ {:➜ok (be ∅) :➜no-alloc (be ∅) :➜collision (be t)})
    ))
(test-hook test-identity-0)

      
