#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package :tm)

(defun test-unwrap-0 ()
  (and
    (equal (unwrap '()) '())
    (equal (unwrap '(1)) '(1))
    (equal (unwrap '(1 2 3)) '(1 2 3))
    (equal (unwrap '((1))) '(1))
    (equal (unwrap '(1 (2 3) 4)) '(1 2 3 4))
    (equal (unwrap '((1 2) (3 (4 (5 6)) 7) 8))  '(1 2 3 (4 (5 6)) 7 8) )
    (equal (unwrap '((1 2) () (3 (4 (5 6)) 7) 8))  '(1 2 3 (4 (5 6)) 7 8) )
    ))
(test-hook test-unwrap-0)    


(defun test-meta-wrap-0 ()
  (and
    (equal (meta-wrap '(1 2 3)) '(list (list 1 2 3)))
    (equal (meta-wrap `(∅ 2 (o (a b) (c d)) 3)) '(list (list ∅ 2) (a b) (c d) (list 3)))
    (equal (meta-wrap `(∅ 2 (o x))) '(list (list ∅ 2) x))
    ))
(test-hook test-meta-wrap-0)


(defun test-q-0 () (equal (q a b c) '(a b c)))
(test-hook test-q-0)


(defun test-q-1 ()
  (equal '(a (b c) d) (q a (b c) d))
  )
(test-hook test-q-1)


(defun test-L-0 ()
  (equal
    (L 1 '(2 3) (o '(4 5)))
    '(1 (2 3) 4 5)
    ))
(test-hook test-L-0)


(defun mk-7 () 7)

(defun test-L-1 ()
  (equal (L (o (q a b mk-7)) (mk-7)) '(a b mk-7 7)))
(test-hook test-L-1)


(defun test-L-2 ()
  (equal {(o (q a b mk-7)) (mk-7)} '(a b mk-7 7)))
(test-hook test-L-2)

