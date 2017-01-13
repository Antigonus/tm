#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; resource contexts
;;
  (def-function-class use-entanglements (tm continuation))

  (defun-typed use-entanglements
    (
      (tm ts-tape-machine)
      continuation
      )
    (let(
          (ent-obj (entanglements tm))
          )
      (acquire-lock (ent-obj-ent-lock ent-obj))
      (acquire-lock (ent-obj-count-lock ent-obj))
      (when (≠ (en-obj-count ent-obj) 0) 
        (condition-wait (ent-obj-count-cond ent-obj) (en-obj-count-lock))
        )
      [continuation ent-obj]
      (release-lock (ent-obj-count-lock ent-obj))
      (release-lock (ent-obj-ent-lock ent-obj))
      ))

  (def-function-class use-head (tm continuation))
  (defun-typed use-head
    (
      (tm ts-tape-machine)
      continuation  ; will be called with tm as we need the (head tm) reference to be intact
      )
    (let(
          (ent-obj (entanglements tm))
          )

      (acquire-lock (ent-obj-ent-lock ent-obj))
      (acquire-lock (ent-obj-count-lock ent-obj))
      (incf (ent-obj-count))
      (release-lock (ent-obj-count-lock ent-obj))
      (release-lock (ent-obj-ent-lock ent-obj))

      [continuation tm]

      (acquire-lock (ent-obj-count-lock ent-obj))
      (decf (ent-obj-count))
      (when (= (en-obj-count ent-obj) 0) 
        (condition-notify (ent-obj-count-cond ent-obj))
        )
      (release-lock (ent-obj-count-lock ent-obj))
      ))


