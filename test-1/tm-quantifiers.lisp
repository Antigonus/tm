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

  (defun test-d*-0 ()
    (let*(
           (a (list 1 2 3))
           (tm1 (mount a))
           )
      (d* tm1)
      (equal
        (tape tm1)
        '(1)
        )))
  (test-hook test-d*-0)

  (defun test-sn-0 ()
    (let*(
           (y '(1 3 6 5))
           (tmy (mount y))
           )
      (and
        (sn tmy 2
          (λ()(= (r tmy) 6))
          (be ∅)
          )
        (sn tmy 27
          (be ∅)
          (λ(cnt)
            (and (= (r tmy) 5) (= cnt 26))
            )))))
  (test-hook test-sn-0)

  (defun test-sn-1 ()
    (let(
          (k0 (mount (list 10 11 12)))
          (k1 (mount (list 13 14 15)))
          )
      (∧
        (= (r k0) 10)
        (sn k0 1)
        (= (r k0) 11)
        (= (sn k0 22 (be ∅) #'echo) 21)

        (= (r k1) 13)
        (sn k1 2)
        (= (r k1) 15)
        (sn k1 1 (be ∅) (be t))
        )))
  (test-hook test-sn-1)

  (defun test-⟳-0 ()
    (let(
          (tm-src (mount [a b c]))
          (tm-dst (tm-mk 'tm-list))
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
        (equal (tape tm-src) (tape tm-dst))
        )))
  (test-hook test-⟳-0)

  (defun test-as*-0 ()
    (let(
          (tm0 (mount {1 2 3}))
          (tm1 (mount [a b c]))
          )
      (s* tm0)
      (s* tm1)
      (as* tm0 (mount {4 5 6}))
      (a*  tm1 (mount [e f g]))
      (and
        (= (r tm0) 6)
        (eq (r tm1) 'c)
        (equal (tape tm0) {1 2 3 4 5 6})
        (equal (tape tm1) [a b c e f g])
        )))
  (test-hook test-as*-0)
