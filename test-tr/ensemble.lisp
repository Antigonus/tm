#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package #:tm)

(defun test-ensemble-0 ()
  (let*(
        (tm0 (mk 'list-tm {:tape {1 2 3 31}}))
        (tm1 (mk 'list-tm {:tape {4 5 6}}))
        (tm2 (mk 'list-tm {:tape {7 8 9 91}}))
        (tm10 (mk 'ensemble-tr {:list {tm0 tm1 tm2}}))
        )
    (∧
      (= (r tm0) 1)
      (= (r tm1) 4)
      (= (r tm2) 7)
      (s tm10)
      (= (r tm0) 2)
      (= (r tm1) 5)
      (= (r tm2) 8)
      (s tm10)
      (= (r tm0) 3)
      (= (r tm1) 6)
      (= (r tm2) 9)
      (¬ (s tm10))
      (eq (r (members tm10)) tm1)
      (on-rightmost (r (members tm10)))

      (h◧ (members tm10))
      (¬ (on-rightmost (r (members tm10))))
      (= (r (r (members tm10))) 3)
      (s (members tm10))
      (on-rightmost (r (members tm10)))
      (= (r (r (members tm10))) 6) 
      (s (members tm10))
      (¬ (on-rightmost (r (members tm10))))
      (= (r (r (members tm10))) 9) 
      (¬ (s (members tm10)))

      (h◧∃ (members tm10)
        (λ(tm ct c∅)
          (if
            (¬ (on-rightmost (r tm)))
            [ct]
            [c∅]
            )))
      )))
(test-hook test-ensemble-0)

      
