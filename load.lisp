#|
  Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
  Released under the MIT License (MIT)
  See LICENSE.txt

|#

(require "asdf")
(pushnew (truename "/home/tm") asdf:*central-registry* :test #'equal)
(asdf:load-system "tm")
(require "tm")

(use-package :tm)

(defun regress-tm ()
  (let(
        (results-0 (test-all-and-self))
        )
    (nl)(princ "tm with test self tests: ")(princ results-0)
    (if
      (equal results-0 {3 82})
      (progn
        (nl)(princ "tm with test self tests passes")
        (nl)(princ "running again without the self tests")
        (let(
              (results-1 (test-all))
              )
          (if
            (equal results-1 {0 78})
            (progn (nl)(princ "tests second run pass"))
            (progn (nl)(princ "tests second run failed"))
            )
            results-1
            ))
      (progn
        (nl)(princ "tm with test self tests failed")
        (nl)(princ "should have been (3 82), but was ")(princ results-0)(princ ".")
        results-0
        ))))
  
