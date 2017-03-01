#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Manager synchronizes operation on tape machines.

|#

(in-package #:tm)

(def-type manager ()
    ))

(def-type managed-tm ()
  (
    (entanglements ; shared machine
      :initarg :entanglements
      :accessor entanglements
      )
    (report-flag
      :initarg :report-flag
      :accessor report-flag
      )
    ))
