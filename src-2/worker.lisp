#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

'worker' is short for 'assembly line worker'.

An assembly line worker connects to zero or more source tape machines, a state variable,
and zero or more destination tape machines. 

The definition of the worker produces a function with the given name, though in this
implementation said function may be called directly, it is conventional that instead the
function call be provided to a connect macro, which returns a step function. 

Upon each call to the step function (or directly to the worker function), a new
destination is written to one or more of the destination tapes.  Each call causes the
write of one result unit.  After the call the destination tape heads are left on the cells
for the new values, so they may be read.

In the simplest case there is one source machine, a state variable, and one destination
machine, and the worker uses #'as to put a new value on the destination machine.  The
value may be read from the destination machine using #'r.  In this case the worker is a
kind of pipe processor.

Though we have a contract about writing the destination (one result per step), in general
no contracts are made about the source machines, though specific workers may make specific
contracts.

Need to add checks such that continuations are always in tail positions in the body
(wrap them in 'return-from funcall ..').  srcs can be destroyed .. but not written?  but
the write could be a mark that means 'read this'.. etc.  At least check that srcs dests
are treated as tape machines in the body.

|#

(in-package #:tm)


;;(defmacro g () (let( (a (gensym))) {'defun 'gg {a} a})) ; (g) (gg 7) -> 7
(defun def-worker-1 (defun-args bindings-at arg)
    (cond
      ((consp arg) ; if arg is a list, then it will need to be detructured
        (let(
              (list-arg-name (gensym))
              )
          (as defun-args list-arg-name)
          (as bindings-at [destructuring-bind])
          (si bindings-at)
          (as bindings-at arg)
          (as bindings-at list-arg-name)
          ))
      (t
        (as defun-args arg)
        )
      ))

(defmacro def-worker (name src dst state conts &body body)
  (let(
        (the-defun (tm-mk 'tm-list {'defun name}))
        (defun-args (tm-mk 'tm-list))
        (bindings (tm-mk 'tm-list))
        )
    (let(
          (bindings-at (dup bindings)) ; the attach point is moved into subspaces
          )

      (when
        (∧ (¬ src)(¬ dst))
        (error 'worker-must-have-src-or-dst)
        )

      (when src (def-worker-1 defun-args bindings-at src))
      (when dst (def-worker-1 defun-args bindings-at dst))

      (when
        (∨  state conts)
        (as defun-args '&optional)
        )
      
      (when state (def-worker-1 defun-args bindings-at state))
      (when conts (def-worker-1 defun-args bindings-at conts))
      
      #|
           (print {"the-defun" (tape the-defun)})
           (print {"defun-args" (tape defun-args)})
           (print {"bindings" (tape bindings)})
      |#

      (s* the-defun)
      (as the-defun (cdr (tape defun-args)))

      (when body
        (if (singleton bindings)
          (as* the-defun (tm-mk 'tm-list body))
          (progn
            (as* bindings-at (tm-mk 'tm-list body))      
            (as the-defun (cadr (tape bindings)))
            )))
      
      ;; `(tape ,the-defun)
      (tape the-defun)
      )))

#|
with prints turned on:

(def-worker alice a (b c) (e) (&optional (cont-ok (be t)) (cont-rightmost (be ∅))))

("the-defun" (DEFUN ALICE)) 
("defun-args" (LIST A #:G528 #:G529 #:G530)) 
("bindings"
 (LIST
  (DESTRUCTURING-BIND
      (B C)
      #:G528
    (DESTRUCTURING-BIND
        (E)
        #:G529
      (DESTRUCTURING-BIND
          (&OPTIONAL (CONT-OK (BE T)) (CONT-RIGHTMOST (BE ∅)))
          #:G530))))) 
T
|#


  


