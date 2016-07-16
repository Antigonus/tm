#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Make nd-tape-machine lists.

  If we inherit from nd-tape-machine, then if we call a function that
  is not defined for list-nd-tm, but is defined for nd-tape-machine, then
  we will get the nd-tape-machine function.  nd-tape machine is purely generic,
  so it in turn will call primitives defined on list-nd-tm.  All is good.

  If we inherit from list-tm, and we call a function not defined for list-nd-tm,
  but is defined for list-tm, we correctly get the list-tm function.

  about the inheritance diamond:

  Should we inherit first from tm-list, and there is a derived function defined both
  for both tm-list and nd-tape-machine, we would get the tm-list version, but
  we will want the nd-tape-machine version - so better do it the other way.

  When we inherit first from nd-tape-machine and there is a derived function defined
  for both tm-list and tape-machine, we better get the list-tm version.  nd-tape-machine
  is inherited from tape-machine, but we should only get that version should their
  be no tm-list version.

|#

(in-package #:tm)


;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass list-nd-tm (nd-tape-machine list-tm)())

    
