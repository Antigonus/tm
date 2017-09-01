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
        (= (plex-channel-channel ch1) 1)
        (= (plex-channel-channel ch2) 2)
        (= (plex-channel-channel ch3) 3)
        (= (plex-channel-channel (parent pt ch1)) 0)
        (= (plex-channel-channel (parent pt ch2)) 0)
        (= (plex-channel-channel (parent pt ch3)) 2)
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


(defun test-plex-2 ()
  (let (pt ch2)
    (get-channel pt) ; forks from channel 0
    (setf ch2 (get-channel pt)) ; forks from channel 0
    (get-channel pt ch2) ; forks from ch2

    (let(a b c)
      (w<plex> a 7)
      (w<plex> b 8)
      (w<plex> c 9)

      (w<plex> c 11 {:channel (channel 2)})
      (w<plex> b 13 {:channel (channel 3)})
      (w<plex> a 15 {:channel (channel 1)})

      (∧
        (= (r<plex> a {:parent-table pt :channel (channel 1)}) 15)
        (= (r<plex> a {:parent-table pt :channel (channel 3)}) 7)

        (= (r<plex> b {:parent-table pt :channel (channel 3)}) 13)
        (= (r<plex> b {:parent-table pt :channel (channel 2)}) 8)

        (= (r<plex> c {:parent-table pt :channel (channel 3)}) 11)
        (= (r<plex> c {:parent-table pt :channel (channel 2)}) 11)
        (= (r<plex> c {:parent-table pt :channel (channel 1)}) 9)
        ))
    ))
(test-hook test-plex-2)

