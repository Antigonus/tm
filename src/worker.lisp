#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

'worker' is short for 'assembly line worker'.

An assembly line worker accepts zero or more source tape machines, a state variable, and
zero or more destination tape machines.  Upon each call to the worker, a new destination
is written to one or more of the destination tapes.  Each call causes the write of one
result unit.  After the call the destination tape heads are left on the cells for the new
values, so they may be read.  A worker may return a value that characterizes the
destination, and may do so via one or more continuations.

In the simplest case there is one source machine, a state variable, and one destination
machine, and the worker uses #'as to put a new value on the destination machine.  The
value may be read from the destination machine using #'r.  In this case the worker is a
kind of pipe processor.

Though we have a contract about writing the destination (one result unit per step), in
general no contracts are made about the source machines, though specific workers may make
specific contracts.

|#

(in-package #:tm)

(defmacro def-worker (name state src dst conts &body body)
  (cond
    ((∧ src dst)
      (cond
        ((∧ (consp src) (consp dst)) ; many-to-many
          `(defun ,name (,state src-list dst-list &optional conts-list)
             (destructuring-bind ,src src-list
               (destructing-bind ,dst dst-list
                 (destructuring-bind ,conts conts-list
                   ,@body
                   )))))

        ((∧ (consp src)) ; many-to-one
          `(defun ,name (,state src-list ,dst &optional conts-list)
             (destructuring-bind ,src src-list
               (destructuring-bind ,conts conts-list
                 ,@body
                 ))))

        ((∧ (consp dst)) ; one-to-many
          `(defun ,name (,state ,src dst-list &optional conts-list)
             (destructuring-bind ,dst dst-list
               (destructuring-bind ,conts conts-list
                 ,@body
                 ))))

        (t ; we can infer one-to-one
          `(defun ,name (,state ,src ,dst &optional conts-list)
             (destructuring-bind ,conts conts-list
               ,@body
               )))
        ))

    (dst ; but no source, worker is a generator
      (cond
        ((∧ (consp dst)) ; generates to many machines
          `(defun ,name (,state dst-list &optional conts-list)
             (destructuring-bind ,dst dst-list
               (destructuring-bind ,conts conts-list
                 ,@body
                 ))))

        (t ; generates to one machine
          `(defun ,name (,state ,dst &optional conts-list)
             (destructuring-bind ,conts conts-list
               ,@body
               )))
        ))
    
    (src ; but no destination, worker is a sink
      (cond
        ((∧ (consp src)) ; sinks from many machines
          `(defun ,name (,state src-list &optional conts-list)
             (destructuring-bind ,src src-list
               (destructuring-bind ,conts conts-list
                 ,@body
                 ))))

        (t ; sinks from one machine
          `(defun ,name (,state ,src &optional conts-list)
             (destructuring-bind ,conts conts-list
               ,@body
               )))
        ))
    
    (t ; worker's purpose is to evolve state, step by step
      `(defun ,name (,state &optional conts-list)
         (destructuring-bind ,conts conts-list
           ,@body
           ))
      )
    ))




