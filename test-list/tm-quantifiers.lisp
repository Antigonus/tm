#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package #:tm)

;;--------------------------------------------------------------------------------
;; tm-quantifiers
;;
  (defun test-∃-0 ()
    (let*(
           (y {1 2 {3 4} 5})
           (ytm (mount y))
           )
      (∃ ytm (λ(tm)(and (typep (r tm) 'cons) (eql 3 (car (r tm))))))
      (equal (r ytm) '(3 4))
      ))
  (test-hook test-∃-0) 

  (defun test-∀-0 ()
    (∧
      (∀ (mount {1 3 5}) (λ(tm)(oddp (r tm))))
      (¬ (∀ (mount {4})  (λ(tm)(oddp (r tm)))))
      ))
  (test-hook test-∀-0)

  (defun test-¬∀-0 ()
    (let*(
           (y '(1 3 4 5))
           (ytm (mount y))
           )
      (¬∀ ytm (λ(tm)(and (numberp (r tm)) (oddp (r tm)))))
      (= (r ytm) 4)
      ))
  (test-hook test-¬∀-0) 

  (defun test-⟳-0 ()
    (let(
          (tm-src (mount [a b c]))
          (tm-dst (mount {1}))
          )
      (labels(
               (worker (cont-ok cont◨)
                 (as tm-dst (r tm-src))
                 (s tm-src
                   cont-ok
                   cont◨
                   ))
               )
        (⟳ #'worker)
        (equal (tape tm-src) (cdr (tape tm-dst)))
        )))
  (test-hook test-⟳-0)

