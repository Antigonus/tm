#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

An aggregate is to data structures what lazy evaluation is to programs.  An aggregate is
tape machine where the instances are again tape machines.  We have two views of 
the aggregate, the aggregate view, and the smooth view.

In the aggregate view, we are just manipulating a list, where the instances are tape
machines.  In the 'smooth-vew' of the aggregate, traversal of cells in one machine of the
agregate from one continues to the machine in the right neighbor, thus the user of the
interface sees just another tape machine for holding arbitrary instances.

The aggregate machine is used for managing the aggregate.  The smooth machine is
what the programmer uses.

When an aggregate consists of tape machines mounted on fixed arrays, the aggregate machine
may insert new tape machine members of the aggregate so as to facilitate data spilling
when emulating allocation at the smooth view level.  Alloction and deallocation can be
achieved for the fixed arrays by creating extensible arrays as neighbors and spilling to
them.  As Lisp extendable arrays only grow to larger indexes, reverse the indexes to
create a left growing array, or implement a double linked list at the aggregate level, and
spill to the left.  Or both. In any case, for now I have not implement alloc or dealloc at
the smooth view level.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; a specialization
;;
  (defclass tm-aggregate (tm-list)())

  (defclass tm-smooth (tape-machine)())


;;--------------------------------------------------------------------------------
;;
  (defmethod tm-init ((instance tm-aggregate) init-list)
    (if 
      (¬ init-list)
      (let(
            (attach-point (tm-mk 'tm-singular-projective 'tm-aggregate))
            )
        (call-next-method instance {{attach-point}})
        )
      (let(
            (init (car init-list))
            )
        (cond
          ((every (λ(i)(typep i 'tape-machine)) init-list)   
            (call-next-method instance {init-list})
            )
          ((∧ 
             (¬ (cdr init-list)) 
             (consp init)
             (every (λ(i)(typep i 'tape-machine)) init)
             )
            (call-next-method instance init-list)
            )
          (t
            (error 'tm-mk-bad-init-type)
            )))))

  (defmethod tm-init ((instance tm-smooth) init-list)
    (when 
      (¬ init-list) 
      (error 'tm-mk-bad-init-type :text "tm-smooth requires an initializer")
      )
    (let(
          (init (car init-list))
          )
      (cond
        ((∧ 
           (¬ (cdr init-list)) 
           (typep init 'tm-aggregate)
           )
          (setf (tape instance) init)
          instance
          )
        (t 
          (error 'tm-mk-bad-init-type)
          ))))



 
  
