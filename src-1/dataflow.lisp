#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#


;;--------------------------------------------------------------------------------
;; synchronizing inputs
;;
  (defun synch
    (
      pred
      tms 
      &optional 
      (cont-ready (be t))
      (cont-not-ready (λ(retry tms)(declare (ignore retry tms))(error 'not-ready)))
      )
    (labels(
             (retry ()
               (∀ (mount tms) pred
                 cont-ready
                 (λ()(funcall cont-not-ready #'retry tms))
                 ))
             )
      (retry)
      ))
