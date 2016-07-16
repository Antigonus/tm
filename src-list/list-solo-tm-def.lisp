#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make solo-tape-machine lists.

  Should we inherit first from tm-list, and there is a derived function defined both
  for both tm-list and solo-tape-machine, we would get the tm-list version, but
  we will want the solo-tape-machine version! -- so better do it the other way.

  When we inherit first from solo-tape-machine and there is a derived function defined
  for both tm-list and tape-machine, we better get the list-tm version.  solo-tape-machine
  is inherited from tape-machine, but we should only get that version should their
  be no tm-list version.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass list-solo-tm (solo-tape-machine list-tm)())

    
