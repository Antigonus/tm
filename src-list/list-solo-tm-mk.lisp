#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make list machines.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; making list machines from other objects
;;

  ;; the init-value as a cons cell falls back to list-tm version.

  ;; init-value as another machine is not supported - solo machines never have
  ;; multiple heads on the same tape.
