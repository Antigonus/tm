#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
|#

(in-package #:le)

(defun newline ()
  "no inputs, outputs a string that is just a newline"
    (let(
          (r (mk-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
          )
      (with-output-to-string (stream r)
        (format stream "~%")
        r
        )))

(defun string-∅ (s)
  (string= s "")
  )

;; no need for ->string as CL already has wwith-output-to-string,  write-to-string etc.

(defun add-quotes (str) 
  (concatenate 'string "\"" str "\"")
  )
(defun add-escaped-quotes (str)  
  (concatenate 'string "\\\"" str "\\\"")
  )
(defun add-squotes (str) 
  (concatenate 'string "'" str "'")
  )
(defun add-escaped-squotes (str)
  (concatenate 'string "\\'" str "\\'")
  )
  
;; can be used, for example, for dropping one nest of quotes or parens
;; similar to trim
(defun drop-ends (s)
  "input is a sequence, output is a sequence missing the first and last member"
  (subseq s 1 (1- (length s)))
  )
(defun drop-ends-test-0 ()
  (and
    (equal "abc" (drop-ends "\"abc\""))
    (equal '(2 3) (drop-ends '(1 2 3 4)))
  ))
(test-hook drop-ends-test-0)

;; works in general for sequences
;; it appears that strings in lisp are not computational, ie. one is forced to first
;; calculate the length of at least the prefix before doing a prefix compare. So if it is
;; long, we wait for this first.  (or does common lisp keep the length separately).  So we
;; get two runs through the prefix. If loop would just give us the leftovers, or tell us
;; which string kicked us out of the loop, then a prefix compare could be computational.
;;
  (defun is-prefix (s p)
    (let(
          (length-of-p (length p))
          (length-count 0)
          )
      (loop 
        for sc across s
        for pc across p
        do 
        (incf length-count)
        (unless (eql sc pc) (return-from is-prefix ∅))
        )
      (eql length-of-p length-count)
      ))

  (defun test-is-prefix ()
    (let(
          (s1 "")
          (s2 "abcdef")
          (s3 "a")
          (p1 "")
          (p2 "abcd")
          (p3 "abce")
          )
      (and
        (is-prefix s1 p1)
        (is-prefix s2 p1)
        (is-prefix s2 p2)
        (is-prefix s3 p1)
        (not (or
               (is-prefix s1 p2)
               (is-prefix s3 p2)
               (is-prefix s2 p3)
               )))))
  (test-hook test-is-prefix)

;; subseq is non-destructive, so this operation could take a long time!
;;
  (defmacro drop (s &optional (count 1))
    `(let*(
            (length-s (length ,s))
            (local-count (if (> ,count length-s) length-s ,count))
            )
       (setq ,s (subseq ,s local-count))
       ))

  (defun test-drop-0 ()
    (let(
          (s0 "abc")
          (s1 "a")
          (s2 "xy")
          )
      (drop s0 0)
      (drop s0 1)
      (drop s1 7)
      (drop s2 2)
      (and
        (string= s0 "bc")
        (string= s1 "")
        (string= s2 "")
        )))
  (test-hook test-drop-0)

