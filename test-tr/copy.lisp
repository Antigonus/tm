#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)

(defun test-copy-shallow-0 ()
  (let(
        (src (mk 'list-tm {:tape {1 2 3 4}}))
        (dst (mk 'list-tm {:tape (q a b c d e f)}))
        )
    (∧
      (=
        (copy-shallow src dst
          {
            :➜ok (be 10) 
            :➜dst-full (be 11)
            :➜src-depleted (be 12)
            })
        12
        )
      (equal (tape dst) (q 1 2 3 4 e f))
      (eq (r src) 4)
      (eq (r dst) 4)
      )))
(test-hook test-copy-shallow-0)

(defun test-copy-shallow-1 ()
  (let(
        (src (mk 'list-tm {:tape (q a b c d e f)}))
        (dst (mk 'list-tm {:tape {1 2 3 4}}))
        )
    (∧
      (=
        (copy-shallow src dst {:➜ok (be 10) :➜dst-full (be 11) :➜src-depleted (be 12)})
        11
        )
      (equal (tape dst) (q a b c d))
      (eq (r src) 'd)
      (eq (r dst) 'd)
      )))
(test-hook test-copy-shallow-1)

(defun test-copy-shallow-2 ()
  (let(
        (src (mk 'list-solo-tm {:tape {1 2 3 4}}))
        (dst (mk 'list-solo-tm {:tape (q a b c d e f)}))
        )
    (copy-shallow-fit src dst)
    (equal (tape dst) (q 1 2 3 4))
    ))
(test-hook test-copy-shallow-2)

(defun test-copy-shallow-3 ()
  (let(
        (src (mk 'list-tm {:tape (q a b c d e f)}))
        (dst (mk 'list-tm {:tape {1 2 3 4}}))
        )
    (copy-shallow-fit src dst)
    (equal (tape dst) (q a b c d e f))
    ))
(test-hook test-copy-shallow-3)
