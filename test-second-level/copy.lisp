#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)

(defun test-c-status-0 ()
  (let(
        (src (mk 'status-tm {:base-type 'list-tm :tape {1 2 3 4}}))
        (dst (mk 'status-tm {:base-type 'list-tm :tape (q a b c d e f)}))
        )
    (∧
      (=
        (c src dst
          {
            :➜ok (be 10) 
            :➜dst-full (be 11)
            :➜src-depleted (be 12)
            })
        12
        )
      (equal (tape (base dst)) (q 1 2 3 4 e f))
      (eq (r src) 4)
      (eq (r dst) 4)
      )))
(test-hook test-c-status-0)

(defun test-c-status-1 ()
  (let(
        (src (mk 'status-tm {:base-type 'list-tm :tape (q a b c d e f)}))
        (dst (mk 'status-tm {:base-type 'list-tm :tape {1 2 3 4}}))
        )
    (∧
      (=
        (c src dst {:➜ok (be 10) :➜dst-full (be 11) :➜src-depleted (be 12)})
        11
        )
      (equal (tape (base dst)) (q a b c d))
      (eq (r src) 'e)
      (eq (r dst) 'd)
      )))
(test-hook test-c-status-1)

(defun test-c-status-2 ()
  (let(
        (src (mk 'status-tm {:base-type 'list-solo-tm :tape {1 2 3 4}}))
        (dst (mk 'status-tm {:base-type 'list-solo-tm :tape (q a b c d e f)}))
        )
    (c-fit src dst)
    (equal (tape (base dst)) (q 1 2 3 4))
    ))
(test-hook test-c-status-2)

(defun test-c-status-3 ()
  (let(
        (src (mk 'status-tm {:base-type 'list-tm :tape (q a b c d e f)}))
        (dst (mk 'status-tm {:base-type 'list-tm :tape {1 2 3 4}}))
        )
    (c-fit src dst)
    (equal (tape (base dst)) (q a b c d e f))
    ))
(test-hook test-c-status-3)

(defun test-c-status-4 ()
  (let(
        (src (mk 'ea-tm {:base-type 'list-haz-tm :status 'empty}))
        (dst (mk 'ea-tm {:base-type 'list-haz-tm :tape (q a b c d e f)}))
        )
    (c-fit src dst)
    (typep dst 'empty)
    ))
(test-hook test-c-status-4)

(defun test-c-status-5 ()
  (let(
        (src (mk 'status-tm {:base-type 'list-solo-tm :tape {1 2 3 4} :status 'parked}))
        (dst (mk 'status-tm {:base-type 'list-solo-tm :tape (q a b c d e f)}))
        )
    (c-fit src dst)
    (equal (tape (base dst)) (q 1 2 3 4))
    ))
(test-hook test-c-status-5)

(defun test-c-status-6 ()
  (let(
        (src (mk 'status-tm {:base-type 'list-solo-tm :tape {1 2 3 4} :status 'parked}))
        (dst (mk 'status-tm {:base-type 'list-solo-tm :status 'empty}))
        )
    (c-fit src dst)
    (equal (tape (base dst)) (q 1 2 3 4))
    ))
(test-hook test-c-status-6)

(defun test-c-status-7 ()
  (let(
        (src (mk 'status-tm {:base-type 'list-solo-tm :tape {1 2 3 4}}))
        (dst (mk 'status-tm {:base-type 'list-solo-tm :status 'empty}))
        )
    (c-fit src dst)
    (equal (tape (base dst)) (q 1 2 3 4))
    ))
(test-hook test-c-status-7)
