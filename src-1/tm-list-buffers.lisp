#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a more specific stack interface
;;
  (defclass stack-list (stack tm-list)())

  (defmethod tm-init ((instance stack-list) init-list)
    (change-class instance 'tm-list)
    (tm-init instance init-list)
    (change-class instance 'stack-list)
    instance
    )

;;--------------------------------------------------------------------------------
;; a more specific queue interface
;;
  (defclass queue-list (queue tm-list)())

  (defmethod tm-init ((instance queue-list) init-list)
    (change-class instance 'tm-list)
    (tm-init instance init-list)
    (change-class instance 'queue-list)
    instance
    )

