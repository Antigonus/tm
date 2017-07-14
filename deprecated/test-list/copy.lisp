#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)

(defun test-c-0 ()
  (let(
        (src (mk 'list-tm {:tape {1 2 3 4}}))
        (dst (mk 'list-tm {:tape (q a b c d e f)}))
        )
    (∧
      (=
        (c src dst
          {
            :➜ok (be 10) 
            :➜src-depleted (be 11)
            :➜dst-full (be 12)
            })
        11
        )
      (equal (tape dst) (q 1 2 3 4 e f))
      (eq (r src) 4)
      (eq (r dst) 4)
      )))
(test-hook test-c-0)

(defun test-c-1 ()
  (let(
        (src (mk 'list-tm {:tape (q a b c d e f)}))
        (dst (mk 'list-tm {:tape {1 2 3 4}}))
        )
    (∧
      (=
        (c src dst {:➜ok (be 10) :➜src-depleted (be 11) :➜dst-full (be 12)})
        12
        )
      (equal (tape dst) (q a b c d))
      (eq (r src) 'e)
      (eq (r dst) 'd)
      )))
(test-hook test-c-1)

(defun test-c-2 ()
  (let(
        (src (mk 'list-solo-tm {:tape {1 2 3 4}}))
        (dst (mk 'list-solo-tm {:tape (q a b c d e f)}))
        )
    (c-fit src dst)
    (equal (tape dst) (q 1 2 3 4))
    ))
(test-hook test-c-2)

(defun test-c-3 ()
  (let(
        (src (mk 'list-tm {:tape (q a b c d e f)}))
        (dst (mk 'list-tm {:tape {1 2 3 4}}))
        )
    (c-fit src dst)
    (equal (tape dst) (q a b c d e f))
    ))
(test-hook test-c-3)
