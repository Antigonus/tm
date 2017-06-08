#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|# 

(in-package #:tm)

(defun mk<tape> (tape-class init)
  (let(
        (tape-instance (make-instance tape-class))
        )
    (init<tape> instance init)
    ))

;; (➜ok #'echo) (➜bad (λ()(error 'bad-init-value))) (➜no-alloc #'alloc-fail)
(def-function-class init (tape-instance init &optional ➜))

