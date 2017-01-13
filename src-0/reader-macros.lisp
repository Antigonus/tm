#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
  (in-package #:tm)

;;--------------------------------------------------------------------------------
;;  { .. }  reader macro for (L ..)
;;
  (defun braces-reader-macro (stream char)
    (declare (ignore char))
    (cons 'L (read-delimited-list #\} stream t))
    )
  (set-macro-character #\{ #'braces-reader-macro)
  (set-macro-character #\} (get-macro-character #\) nil))


;;--------------------------------------------------------------------------------
;;  [ .. ]  reader macro for (funcall  ..)
;;
  (defun brackets-reader-macro (stream char)
    (declare (ignore char))
    (cons 'funcall (read-delimited-list #\] stream t))
    )
  (set-macro-character #\[ #'brackets-reader-macro)
  (set-macro-character #\] (get-macro-character #\) nil))

