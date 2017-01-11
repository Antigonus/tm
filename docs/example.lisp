(defmacro defsynonym (old-name new-name)
  "Define OLD-NAME to be equivalent to NEW-NAME when used in the first position of a Lisp form."
  `(defmacro ,new-name (&rest args) `(,',old-name ,@args))
  )
(defsynonym lambda λ)
(defun echo (&rest x) (apply #'values x))
(defun be (&rest it) 
  (λ (&rest args)(declare (ignore args))(apply #'values it))
  )

(defun mock-read (n)
  (if (= n 0)
    'zero
    (if (oddp n) 
      'fail 
      (write-to-string n)
      )))

(defun use-mock-read (n)
  (let(
        (result (mock-read n))
        )
    (cond
      ((eq result 'zero) (print "failed"))
      ((eq result 'fail) (print "failed"))
      (t (print result))
      )
    t
    ))

(defun mock-read-with-continuations-0 (n cont-ok cont-zero cont-odd)
  (if (= n 0)
    (funcall cont-zero)
    (if (oddp n) 
      (funcall cont-odd)
      (funcall cont-ok (write-to-string n))
      )))

(defun mock-read-with-continuations-1
  (
    n
    &optional
    (cont-ok #'echo)
    (cont-zero (be 'zero))
    (cont-odd (be 'fail))
    )
  (if (= n 0)
    (funcall cont-zero)
    (if (oddp n) 
      (funcall cont-odd)
      (funcall cont-ok (write-to-string n))
      )))


(defun use-mock-read-with-continuations (n)
  (mock-read-with-continuations-0 n
    (λ() (print "failed") t)
    (λ() (print "failed") t)
    (λ(result) (print result))
    ))
