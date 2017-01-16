#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package #:tm)

  (defun test-⟳-0 ()
    (let(
          (tm-src (mk 'list-tm (q a b c)))
          (tm-dst (mk 'list-tm {1}))
          )
      (labels(
               (worker (repeat cont◨)
                 (as tm-dst (r tm-src))
                 (s tm-src
                   repeat
                   cont◨
                   ))
               (cont◨ () 
                 (equal (tape tm-src) (cdr (tape tm-dst)))
                 )
               )
        (⟳ #'worker #'cont◨)
        )))
  (test-hook test-⟳-0)

  (defun test-∃-0 ()
    (let*(
           (y {1 2 {3 4} 5})
           (ytm (mk 'list-tm y))
           )
      (∧
        (∃
          ytm
          (λ(tm cont-true cont-false)
            (if 
              (∧ (typep (r tm) 'cons) (eql 3 (car (r tm))))
              [cont-true]
              [cont-false]
              )))
        (equal (r ytm) '(3 4))
      )))
  (test-hook test-∃-0) 

  (defun test-∃-1 ()
    (let*(
           (y {1 2 {3 4} 5})
           (ytm (mk 'list-tm y))
           )
      (∃
        ytm
        (λ(tm cont-true cont-false)
          (if 
            (∧ (typep (r tm) 'cons) (eql 3 (car (r tm))))
            [cont-true]
            [cont-false]
            ))
        (λ()(equal (r ytm) '(3 4)))
        #'cant-happen
        )))
  (test-hook test-∃-1) 



  (defun test-∀-0 ()
    (∧
      (∀ (mk 'list-tm {1 3 5}) (λ(tm ct c∅)(if (oddp (r tm)) [ct] [c∅])))
      (¬ (∀ (mk 'list-tm {4})  (λ(tm ct c∅)(if (oddp (r tm)) [ct] [c∅]))))
      ))
  (test-hook test-∀-0)

  (defun test-¬∀-0 ()
    (let*(
           (y '(1 3 4 5))
           (ytm (mk 'list-tm y))
           )
      (¬∀ ytm (λ(tm ct c∅)(if (and (numberp (r tm)) (oddp (r tm))) [ct] [c∅])))
      (= (r ytm) 4)
      ))
  (test-hook test-¬∀-0) 


