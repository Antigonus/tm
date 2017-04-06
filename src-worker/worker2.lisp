#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

'worker' is short for 'assembly line worker'.

An assembly line worker connects to zero or more source tape machines, a state variable,
and zero or more destination tape machines.   (We should add an auto-worker, that
just proceses the whole of the source tapes when called.)

    (def-worker name src dst state (continuation*) body*)

    name := name of the worker
    src := tape-machine-name | (tape-machine-name ..) | ()
    dst := tape-machine-name | (tape-machine-name ..) | ()
    state := symbol | (symbol ..) | ()
    continuation := a contination function

|#

(defun append-list (tm arg)
  (if 
    (consp arg)
    (let(
          (arg-tm (mk-tm 'list-nd-tm {:tape arg}))
          )
      (∀* arg-tm (λ(arg-tm)(as tm (r arg-tm))))
      )
    (as tm arg)
    ))

(defmacro def-worker (name src dst state conts &body body)
  (let(
        (tm10      (mk-tm 'list-nd-tm {:tape {∅}}))
        (the-args  (mk-tm 'status-tm {:base tm10 :empty t}))
        )
    (∨ src dst (error 'worker-must-have-src-or-dst))
    (when src (append-list the-args src))
    (when dst (append-list the-args dst))
    (when state (append-list the-args state))
    

    
        (the-defun (mk-tm 'list-nd-tm {:tape {'defun name}}))
      

