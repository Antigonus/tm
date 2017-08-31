#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
(in-package #:tm)

(defun test-tape-array-0 ()
  (let(
        (tp-empty (mk 'empty))
        (tp0 (mk 'empty))
        (tp1 (mk 'empty))
        (tp2 (mk 'empty))
        (tp3 (mk 'empty))
        )
    (∧
      (a◨<tape-array> tp0 0)

      (a◨<tape-array> tp1 0)
      (a◨<tape-array> tp1 1)

      (a◨<tape-array> tp2 0)
      (a◨<tape-array> tp2 1)
      (a◨<tape-array> tp2 2)
      
      (a◨<tape-array> tp3 0)
      (a◨<tape-array> tp3 1)
      (a◨<tape-array> tp3 2)
      (a◨<tape-array> tp3 3)

      (typep tp-empty 'empty)
      (typep tp0 'tape-array-max-0)
      (typep tp1 'tape-array-max-1)
      (typep tp2 'tape-array-max-2)
      (typep tp3 'tape-array-max-n)

      (= (r<tape-array> tp-empty {:➜empty (λ()5)}) 5)
      (= (r<tape-array> tp0 {:➜ok (λ(x)(if (= x 0) 7 12))}) 7)
      (= (r<tape-array> tp0 {:address 1 :➜empty (λ()5)}) 5)

      (= (r<tape-array> tp1) 0)
      (= (r<tape-array> tp1 {:address 1}) 1)
      (= (r<tape-array> tp1 {:address 2 :➜empty (λ()5)}) 5)

      (= (r<tape-array> tp2) 0)
      (= (r<tape-array> tp2 {:address 1}) 1)
      (= (r<tape-array> tp2 {:address 2}) 2)
      (= (r<tape-array> tp2 {:address 3 :➜empty (λ()5)}) 5)

      (= (r<tape-array> tp3) 0)
      (= (r<tape-array> tp3 {:address 1}) 1)
      (= (r<tape-array> tp3 {:address 2}) 2)
      (= (r<tape-array> tp3 {:address 3}) 3)
      (= (r<tape-array> tp3 {:address 4 :➜empty (λ()5)}) 5)

      )))
(test-hook test-tape-array-0)


