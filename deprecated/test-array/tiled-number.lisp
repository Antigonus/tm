#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-tiled-0 ()
  (let(
        (x #xAA55)
        )
    (∧
      (= (shift-left x 4) #xAA5)
      (= (shift-left x 5) #x552)
      (= (shift-left x 15) 1)
      (= (shift-left x 16) 0)
      )))
(test-hook test-tiled-0)

(defun test-tiled-1 ()
  (let(
        (x (mk 'tiled-number-tm {:tape #xCA357}))
        )
    (∧
      (= (r x) #x7)
      (s x)
      (= (r x) #x5)
      (s x)
      (= (r x) #x3)
      (s x)
      (= (r x) #xA)
      (s x)
      (= (r x) #xC)
      (¬ (s x))
      )))
(test-hook test-tiled-1)


(defun test-tiled-2 ()
  (let(
        (x (mk 'tiled-number-tm {:tape #xCA357}))
        )
    (∧
      (sn x 2)
      (= (r x) #x3)
      (-s x)
      (= (r x) #x5)
      (-s x)
      (= (r x) #x7)
      (¬ (-s x))
      )))
(test-hook test-tiled-2)

(defun test-tiled-3 ()
  (let(
        (x (mk 'tiled-number-tm {:tape #xCA357}))
        )
    (∧
      (sn x 2)
      (= (r x) #x3)
      (a x #xf)
      (s x)
      (= (r x) #xf)
;; haven't written d yet, just did the tm-type functions
;;      (d x)
;;      (= (r x) #x7)
;;      (¬ (s x))
      )))
(test-hook test-tiled-3)

(defun test-tiled-4 ()
  (let(
        (x (mk 'tiled-number-tm {:tape #x7}))
        (y (mk 'tiled-number-tm {:tape #x57}))
        (z (mk 'tiled-number-tm {:tape #x357}))
        )
    (∧
      (tape-length-is-one x)
      (¬ (tape-length-is-two x))
      (¬ (tape-length-is-one y))
      (tape-length-is-two y)
      (¬ (tape-length-is-one z))
      (¬ (tape-length-is-two z))
      )))
(test-hook test-tiled-4)
