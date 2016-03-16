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

;; this macro defines a worker function called 'name', a connection function called
;; 'connect-name'. The connection function returns a step function.
;;
(defmacro def-worker (name src-def dst-def state-def conts-def &body body)
  (let(
        (name-str (symbol-name name))
        )
    (let(
          (connect-name (concatenate 'string "connect-" name-str))
          )
      (let(
            (connect-symbol (intern connect-name))
            )

        `(progn
           (defun ,name (src-arg dst-arg state-arg conts-arg)
             (destructuring-bind ,src-def src-arg
               (destructuring-bind ,dst-def dst-arg
                 (destructuring-bind ,state-def state-arg
                   (destructuring-bind ,conts-def conts-arg
                     ,@body
                     )))))

           (defmacro ,connect-symbol (src-arg dst-arg state-arg conts-arg)
             `(λ()
                (,',name ,src-arg ,dst-arg ,state-arg ,conts-arg)
                )))))))

