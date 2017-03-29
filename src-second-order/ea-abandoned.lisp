#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; entanglements support
;;
  (defun-typed entangled-on-same-cell ((tm ea-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (operation-on-abandoned)
    )

  (defun-typed entangled-on-right-neighbor-cell ((tm ea-abandoned) &optional ➜)
    (declare (ignore tm ➜))
    (operation-on-abandoned)
    )

