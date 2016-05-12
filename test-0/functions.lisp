#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#

(in-package :tm)

(defun test-remove-key-pair-0 ()
  (∧
    (equal
      (remove-key-pair {:a 1 :b 2 :c 3} :a)
      {:b 2 :c 3}
      )
    (equal
      (remove-key-pair {:a 1 :b 2 :c 3} :b)
      {:a 1 :c 3}
      )
    (equal
      (remove-key-pair {:a 1 :b 2 :c 3} :c)
      {:a 1 :b 2}
      )
    (equal
      (remove-key-pair {:a 1 :b 2 :c 3} :alpha)
      {:a 1 :b 2 :c 3}
      )
    (equal
      (remove-key-pair {:a 1} :alpha)
      {:a 1}
      )
    (equal
      (remove-key-pair {:a 1} :a)
      ∅
      )
    ))
    
(test-hook test-remove-key-pair-0)
