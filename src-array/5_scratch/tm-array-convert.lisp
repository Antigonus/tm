#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  note (adjustable-array-p a) for checking if an array is adjustable
  this may be needed as (type-of an-array) just returns (vector T size)
  both are also (typep a 'array).

  (array dimension d 0) returns allocation length for 0 dimension of array d, while
  (length d) returns the fill pointer for vector d, as does (fill-pointer d).

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; making other instances from tm-array machines
;;
  (defun-typed unmount ((tm tm-array))(tape tm))
  (defun-typed to-array ((tm tm-array))(tape tm))
