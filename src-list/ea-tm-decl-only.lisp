#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Primitives are generic interface items that must be implemented.  A list container, an
array container, etc. each would have to implement the primitve for that container
type.

In contrast, derived functions are built upon the primitives.  A programmer who wants to
create a tape machine based on a new container type need not implement derived methods, as
the generic version will work (by calling the correct primitives).

|#

(in-package #:tm)


