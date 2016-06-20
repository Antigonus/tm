#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-tm-DE-0 ()
  (let(
        (tm-src (mk 'tm-DE :diffs {0 -1 2}))
        (tm-dst (mk 'tm-list))
        )
    (as tm-dst (r tm-src))
    (s tm-src)
    (as tm-dst (r tm-src))
    (s tm-src)
    (as tm-dst (r tm-src))
    (s tm-src)
    (as tm-dst (r tm-src))
    (equal
      {0 1 4 9}
      (unmount tm-dst)
      )))
(test-hook test-tm-DE-0)

