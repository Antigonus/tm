#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  As per my "Towards a Better Understanding of CAR and CDR paper".

  This is a first pass implementation, just to try it out. We will be adding quantifiers
  derived commands, etc.  .. we need a balanced delimiter quantifier so as to match
  parents etc.

  Need to change parse to parse step. We will need to pass in the name of the machine
  being stepped.

  Should put the access program in a string, so that we can use '(' in it, etc.  .. '('
  would take a tm as a parameter, and stop pushing when ')' ... perhaps '(1' use a number
  so that matches can overlap.

  Need to add contiuations of course.

  To create an access langauge program concatenate together the tm access functions
  desired to be applied in sequence.  These are the primitive functions:

    r
    w
    s
    a
    d

  So:

  (Δ ssw tm 7)

  Will step tm twice and write the value 7. 

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; accepts a string, and two machines
;;
  (defun parse-command (acc-prog)
    (r acc-prog) ; this will get a tad more complex later
    )

  (defun parse (cmd tm args commands)
    (case cmd
      (#\r (as commands {'r tm}))
      (#\w 
        (if 
          args
          (progn 
            (as commands {'w tm (r args)})
            (s args #'do-nothing (λ()(setf args ∅)))
            )
          (error 'Δ-required-arg-missing :text "write requires an instance")
          ))
      (#\s (as commands {'s tm}))
      (#\a 
        (if
          args
          (progn 
            (as commands {'a tm (r args)})
            (s args #'do-nothing (λ()(setf args ∅)))
            )
          (error 'Δ-required-arg-missing :text "alloc requires an instance")
          ))
      (#\d (as commands {'d tm}))
      (otherwise
        (error 'Δ-unrecognized-command :text (string cmd))
        )
      ))

;;--------------------------------------------------------------------------------
;;             
  (defmacro Δ (acc-prog-string act-on-this-tm &rest args)
    (cond
      ((¬ (stringp acc-prog-string))
        (error 'Δ-malformed-prog-symbol :text "access program must be a string")
        )
      (t
        (let(
              (acc-prog (mount acc-prog-string))
              (tm-commands (mount [progn]))
              (tm-args (if args (mount args) ∅))
              )
          (⟳(λ(cont-loop cont-return)
             (let(
                   (cmd (parse-command acc-prog))
                   )
               (parse cmd act-on-this-tm tm-args tm-commands)
               (s acc-prog cont-loop cont-return)
               )))
          ;;`(pprint (unmount ,tm-commands))
          (unmount tm-commands)
          ))))

