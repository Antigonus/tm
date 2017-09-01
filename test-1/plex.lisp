#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-plex-0 ()
  (let (pt)
    (let*(
          (ch1 (get-channel pt)) ; forks from channel 0
          (ch2 (get-channel pt)) ; forks from channel 0
          (ch3 (get-channel pt ch2)) ; forks from ch2
          )
      (∧
        (= ch1 1)
        (= ch2 2)
        (= ch3 3)
        (= (parent pt ch1) 0)
        (= (parent pt ch2) 0)
        (= (parent pt ch3) 2)
        ))))
(test-hook test-plex-0)
    
(defun test-plex-1 ()
  (let(a b c)
    (w<plex> a 7)
    (w<plex> b 8)
    (w<plex> c 9)
    (= (r<plex> a) 7)
    (= (r<plex> b) 8)
    (= (r<plex> c) 9)
    ))
(test-hook test-plex-1)

