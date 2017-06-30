#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt


|# 

(in-package #:tm)

;; (➜ok #'echo) (➜bad (λ()(error 'bad-init-value))) (➜no-alloc #'alloc-fail)
(def-function-class init (tape-instance init &optional ➜))

(defun mk (tape-type init &optional ➜)
  (let(
        (tape-instance (make-instance tape-type))
        )
    (init tape-instance init ➜)
    ))


