#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

'worker' is short for 'assembly line worker'.

An assembly line worker connects to zero or more source tape machines, a state variable,
and zero or more destination tape machines. The connection returns a step function.  Upon
each call to this step function, one result is computed and written to the destination
tapes.

Though we have a contract about writing a result per step to the destination, in general
no contracts are made about consuming values from the source machines, though specific
workers may make specific contracts.

|#

(in-package #:tm)

;; this macro defines a worker function called 'name',  a connection
;; function called 'connect-name', and a step function called s-name.
;;
(defmacro def-worker (name src dst args conts &body body)
  (let(
        (name-str (symbol-name name))
        )
    (let(
          (connect-name (concatenate 'string "connect-" name-str))
          (step-name (concatentate 'string "s-" name-str))
          )
      (let(
            (connect-symbol (intern connect-name))
            (step-symbol (intern step-name))
            )
        `(progn

           (defun ,name (,src ,dst &optional ,args ,conts)
             ,@body
             )

           (defun ,connect-symbol (,src ,dst ,args ,conts)
             (setf (symbol-function ,step-symbol) 
               (λ()
                 (funcall ,name ,src ,dst ,args ,conts)
                 )))
           )))))
           





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




