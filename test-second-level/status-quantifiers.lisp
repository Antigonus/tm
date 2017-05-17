#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-status-quantifiers-0 ()
  (let*(
         (tm11 (mk 'status-tm {:base-type 'list-solo-tm :tape {21 23 25} :status 'parked}))
         (tm21 (mk 'status-tm {:base-type 'list-solo-tm :status 'empty}))
         )
    (∧
      (typep tm11 'parked)
      (= (r tm11 {:➜ok #'cant-happen :➜parked (be 777)}) 777)
      (=
        (◧∀ tm11 (λ(tm ct c∅) (if (oddp (r tm)) [ct] [c∅])) {:➜t (be 717) :➜∅ (be 719)})
        717
        )
      (=
        (∀ tm21 (λ(tm ct c∅) (if (oddp (r tm)) [ct] [c∅])) {:➜t (be 717) :➜∅ (be 719)})
        717
        )
      (=
        (∃ tm21 (λ(tm ct c∅) (if (oddp (r tm)) [ct] [c∅])) {:➜t (be 717) :➜∅ (be 719)})
        719
        )
      )))
(test-hook test-status-quantifiers-0)


(defun test-status-quantifiers-1 ()
  (let*(
         (tm11 (mk 'status-tm {:base-type 'list-solo-tm :tape {21 23 25} :status 'parked}))
         )
    (∧
      (typep tm11 'parked)
      (= (r tm11 {:➜ok #'cant-happen :➜parked (be 777)}) 777)
      (=
        (∀ tm11 ; quantifier starts at the parked head position
          (λ(tm ct c∅)
            (if
              (∨
                (on-rightmost tm)
                (oddp (esr tm))
                )
              [ct]
              [c∅]
              ))
          {
            :➜t (be 717)
            :➜∅ (be 719)
            }
          )
        717
        )
      )))
(test-hook test-status-quantifiers-1)

