#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#
(in-package #:tm)


;;--------------------------------------------------------------------------------
;; general
;;
  (defun is-parked (tm) (eq (state tm) parked))
  (defun is-void (tm) (eq (state tm) void))
  (defun is-active (tm) (eq (state tm) active))
  (defun is-abandoned (tm) (eq (state tm) abandoned))


