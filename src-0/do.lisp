#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  This is the tm library's basic loop form.  We call it "do", and use the symbol '⟳'
  for the function name.  We prefer that programmers use quantification rather than
  our 'do' function, as they are less error prone.

  When evaluating #'⟳, we pass it exactly one argument, a function, which for sake of
  discussion here we will call #'work. This function may be a nameless lamda.

  #'work will be handed one argument.  That argument is also a function.  Let's call this
  second function #'again.  #'again takes no arguments.  When #'again is evaluated it
  directly calls #'work.

  This is similar to self recursion on #'work, however, we have additional constraints:
  1. that the self recursion function accepts no arguments, (or only accepts #'again when
  using ⟳), 2) that the stack must not be used.  It follows that the only variables that
  can be changed are found in the call context.  Also note that without a structure such
  #'⟳, it is difficult to perform self recursion on a nameless lambda in Common Lisp.

  Such a self recursion with these constraints is the definition of looping in all
  conventional structured languages.  What is known as the 'loop body' is the #'work
  function for #'⟳. A 'continue' statement is just a call to #'again, and a 'break'
  statement is just a return from #'work.

|#

(in-package #:tm)

#| In this form of #'⟳, sbcl didn't always optimize and remove the stack usage, resulting
   in stack overflows.

  (defun ⟳ (work)
    "⟳ (pronounced \"do\") accepts a 'work' function. 'work' may be a nameless lambda. ⟳
         calls work with a single continuation argument.  That continuation is the work
         function.  Hence, when the continuation is called, work is called again.
         "
    (labels(
             (again() (funcall work #'again))
             )
      (again)
      ))
|#

    (defun ⟳ (work)
      "⟳ (pronounced \"do\") accepts a 'work' function. 'work' may be a nameless lambda. ⟳
       calls work with a single continuation argument.  That continuation is the work
       function.  Hence, when the continuation is called, work is called again.
       "
      ;; unlike all other forms in Common Lisp, tagbody does not return the value
      ;; from the last contained body form, instead it always returns nil, so we
      ;; must capture the return-value on every loop, while hoping it will be the 
      ;; last time we must do so.
      (let(return-value) 
        (tagbody 
          again
          (setf return-value (funcall work (λ()(go again))))
          )
        return-value
        ))


