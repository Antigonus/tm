
;;--------------------------------------------------------------------------------
;;
  (defmacro defsynonym (old-name new-name)
    "Define OLD-NAME to be equivalent to NEW-NAME when used in the first position of a Lisp form."
    `(defmacro ,new-name (&rest args) `(,',old-name ,@args))
    )
  (defsynonym lambda λ)

  (defun braces-reader-macro (stream char)
    (declare (ignore char))
    (cons 'list (read-delimited-list #\} stream t))
    )
  (set-macro-character #\{ #'braces-reader-macro)
  (set-macro-character #\} (get-macro-character #\) nil))

  (defun brackets-reader-macro (stream char)
    (declare (ignore char))
    (cons 'funcall (read-delimited-list #\] stream t))
    )
  (set-macro-character #\[ #'brackets-reader-macro)
  (set-macro-character #\] (get-macro-character #\) nil))

  (defun echo (&rest x) (apply #'values x))
  (defun be (&rest it) 
    (λ (&rest args)(declare (ignore args))(apply #'values it))
    )

;;--------------------------------------------------------------------------------
;;
  (defun mock-read (n)
    (if (= n 0) ; zero marks end of stream
      'eos
      (if (oddp n) ; require even parity
        'parity
        (write-to-string n)
        )))

  (defun use-mock-read (n)
    (let(
          (result (mock-read n))
          )
      (case result
        (eos (print "end of stream"))
        (parity (print "parity error"))
        (otherwise (print result))
        )
      t
      ))

  (defun mock-read-with-continuations (n  &optional ➜)
      (destructuring-bind
        (&key
          (➜ok #'echo) ; if all goes 'ok'
          (➜eos (be 'eos)) ; we don't like zeros!
          (➜parity (be 'parity))
          &allow-other-keys
          )
        ➜
        (if (= n 0) ; zero byte marks end of stream
          [➜eos]
          (if (oddp n) ; parity check
            [➜parity] 
            [➜ok (write-to-string n)]
            ))))


  (defun use-mock-read-with-continuations (n)
    (mock-read-with-continuations n
      {
        :➜ok     (λ(result) (print result))
        :➜eos    (λ() (print "end of stream") t)
        :➜parity (λ() (print "parity error") t)
        }))



#|
edition 0

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
|#













