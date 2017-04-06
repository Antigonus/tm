

(in-package :tm)

(defun interesting-0 ()
  (let(
        (i (mk-interval 0 9))
        (N (mk-Natural))
        (count-N 0)
        (count-odd 0)
        (count-even 0)
        )
    (∀* i (λ(i)
            (declare (ignore i))
            (incf count-N)
            (if (oddp (r N)) (incf count-odd) (incf count-even))
            (s N)
            ))
    (∧
      (= count-odd count-even)
      (≠ count-N count-odd)
      )))


(defun interesting-1 ()
  (let(
        (N (mk-Natural))
        (count-N 0)
        (count-odd 0)
        (count-even 0)
        )
    (∀* N (λ(N)
            (incf count-N)
            (if (oddp (r N)) (incf count-odd) (incf count-even))
            ))
    (= count-N count-odd count-even)
    ))
