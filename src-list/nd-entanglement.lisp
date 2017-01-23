#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)


;;--------------------------------------------------------------------------------
;; context with an entangled machine
;;  
  (def-function-class with-mk-entangled (tm continuation &rest ⋯)
    (:documentation
      "Calls continuation with a locally scoped entangled copy of tm.
       "))

  ;; this becomes more interesting when we have entanglement accounting
  (defun-typed with-mk-entangled
    (
      (tm0 nd-tape-machine)
      continuation
      &rest ⋯
      )
    (declare (ignore ⋯))
    (let(
          (tm1 (mk (type-of tm0) tm0))
          )
      (funcall continuation tm1)
      ))
