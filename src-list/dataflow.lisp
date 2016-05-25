#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; synchronizing inputs
;;
  (defun synch
    (
      pred
      tms 
      &optional 
      (cont-ready (be t))
      (cont-not-ready 
        (λ(retry tms)
          (declare (ignore retry tms))
          (error 'not-ready)
          )))
    (labels(
             (retry ()
               (∀ (mount tms) (λ(tm)(funcall pred (r tm)))
                 cont-ready
                 (λ()(funcall cont-not-ready #'retry tms))
                 ))
             )
      (retry)
      ))
