#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A tape-array.

Typically starts out with a length of zero or one, and then grows slowly via a◨.

Designed for storage critical applications, as it doesn't take up any more storage
than necessary.  This makes growing a bit expensive, as the whole array is copied
each time a cell is added.

Common Lisp knows the length of an array, and that information must be stored somewhere.
Hence I would like to expand the tape from being a single instance, a pair instances held in
a struct, perhaps a triple held in a struct, then an array.  To support such a progression
with CLOS, we must distinguish a pair, struct, or an array when they appear as the single
instance, from the pair, struct, or array we use for expansion. I don't see a reasonable
way to do this, as common Lisp isn't giving me a way to alias a type, and if I make a wrapper
type it looks like there is a yet another layer of indirection or worse.  I suppose one could
type-of, create a new type, and change-type ... nah.

One good thing about making tape array an array is that nil is not a valid value.  I use
this in the cell list to distringuish when there are no contents for a given universe id, and
thus we should check the parents table for an inherited value.

In this implementation we just use a simple array and copy and expand as needed.  In the
single instance case, where the single instance is a number or a referece, we pay big with
100% overhead.


|# 

(in-package #:tm)

(defun make<tape-array> (&optional (length 0) (init ∅)) (make-array length :initial-element init))

(demacro expand<tape-array> (tape-array max-address)
  `(let(
         (length (length ,tape-array))
         )
     (when
       (≥ ,max-address length)
       (let(
             (new-tape-array (make-array (1+ ,max-address)))
             )
         (loop for address below length do
           (setf (aref new-tape-array address) (aref ,tape-array))
           )
         (loop for address upto ,max-address do
           (setf (aref new-tape-array address) ∅)
           )))))

;; hopefully the compiler is smart enough to eliminate the array access in the default case
(defmacro r<tape-array> (tape-array &optional (address 0))
  `(aref ,tape-array ,address)
  )
(defmacro w<tape-array> (tape-array instance &optional (address 0))
  `(setf (aref ,tape-array ,address) ,instance)
  )

;; need to add the ➜ code because we can have an allocation error
(defmacro a◨<tape-array> (tape-array instance &optional ➜)
  `(let*(
          (length         (length ,tape-array))
          (new-tape-array (make-array (1+ ,length)))
          )
     (loop for address below length do
       (setf (aref new-tape-array address) (aref ,tape-array address))
       )
     (setf (aref ,new-tape-array length) ,instance)
     (setf ,tape-array new-tape-array)
     ))
