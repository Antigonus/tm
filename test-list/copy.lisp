#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
  (in-package #:tm)

  (defun test-copy-shallow-0 ()
    (let(
          (tm-src (mk 'list-tm {:tape {1 2 3 4}}))
          (tm-dst (mk 'list-tm {:tape (q a b c d e f)}))
          )
      (∧
        (=
          (copy-shallow tm-src tm-dst
            {
              :➜ok (be 10) 
              :➜dst-full (be 11)
              :➜src-depleted (be 12)
              })
          12
          )
        (equal (tape tm-dst) (q 1 2 3 4 e f))
        (eq (r tm-src) 4)
        (eq (r tm-dst) 4)
        )))
  (test-hook test-copy-shallow-0)

  (defun test-copy-shallow-1 ()
    (let(
          (tm-src (mk 'list-tm {:tape (q a b c d e f)}))
          (tm-dst (mk 'list-tm {:tape {1 2 3 4}}))
          )
      (∧
        (=
          (copy-shallow tm-src tm-dst {:➜ok (be 10) :➜dst-full (be 11) :➜src-depleted (be 12)})
          11
          )
        (equal (tape tm-dst) (q a b c d))
        (eq (r tm-src) 'e)
        (eq (r tm-dst) 'd)
        )))
  (test-hook test-copy-shallow-1)

