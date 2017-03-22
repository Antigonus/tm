#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-ts1-0 ()
  (let*(
         (tm0 (mk 'list-haz-tm {:tape {1 2 3}}))
         (tm1 (mk 'ts1-tm {:base tm0}))
         (tm2 (entangle tm1))
         )
    (∧
      (typep tm1 'status-active)
      (typep tm2 'status-active)
      (= (r tm1) 1)
      (= (r tm2) 1)
      (= (address tm1) 0)
      (= (address tm2) 0)
      (= (address-rightmost tm1) 2)
      (= (address-rightmost tm2) 2)
      (= (d tm1) 2)
      (= (address-rightmost tm1) 1)
      (= (address-rightmost tm2) 1)
      (s tm2)
      (= (r tm2) 3)
      (= (address tm2) 1)
      (clean-entanglements tm1)
      )))
(test-hook test-ts1-0)

(defun test-ts1-1 ()
  (let*(
         (data (loop for i from 1 to 10 collect i))
         (tm10 (mk 'list-solo-tm {:tape (append data {'end})}))
         (tm20 (mk 'list-haz-tm {:tape {∅}}))
         (tm21 (mk 'ts1-tm {:base tm20 :empty t}))
         (tm30 (mk 'list-solo-tm {:tape {∅}}))
         (tm31 (mk 'status-tm {:base tm30 :empty t}))
         )
    (let(
          (t1 (bt:make-thread 
                (λ()
                  (∀* tm10 (λ(tm10)(as tm21 (r tm10))))
                  )))
          (t2 (bt:make-thread
                (λ()
                  (⟳
                    (λ(again)
                      (d◧ tm21 tm30
                        {
                          :➜ok (λ(instance)
                                 (when
                                   (¬ (eq instance 'end))
                                   [again]
                                   ))
                          :➜rightmost (λ()
                                        (sleep .001)
                                        [again]
                                        )
                          }))))))
          )
      (bt:join-thread t1)
      (bt:join-thread t2)
      (c◧ tm21)
      (c◧ tm31)
      (⟳
        (λ(again)
          (when
            (¬ (eq (r tm21) (r tm31)))
            (return-from test-ts1-1 ∅)
            )
          (let(
                (c0 (s tm21))
                (c1 (s tm31))
                )
            (if
              (∧ c0 c1)
              [again]
              (return-from test-ts1-1 (∧ (¬ c0) (¬ c1)))
              )))))))

(test-hook test-ts1-1)
