#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

  Tests may also be scattered through the code.

  All tests have names of the form test-fun-n  where fun is the function
  or logical concept being tested.

|#
  (in-package #:tm)


(defun test-state-0 ()
  (let*(
         (a (mk 'tm-void))
         (b (mount {1 2 3}))
         (c (fork-0 b))
         (d (fork-0 b))
         )
    (void c)
    (park d)
    (∧
      (eq (state a) void)
      (eq (state b) active)
      (eq (state c) void)
      (eq (state d) parked)

      (r a (λ(object)(declare (ignore object))∅) (be t))
      (r b (λ(object)(= object 1)) (be ∅))
      (r c (λ(object)(declare (ignore object))∅) (be t))
      (r d (λ(object)(declare (ignore object))∅) (be t))

      (w a 4 (be ∅) (be t))
      (w b 5 (be t) (be ∅))
      (w c 6 (be ∅) (be t))
      (w d 6 (be ∅) (be t))

      (r a (λ(object)(declare (ignore object))∅) (be t))
      (r b (λ(object)(= object 5)) (be ∅))
      (r c (λ(object)(declare (ignore object))∅) (be t))
      (r d (λ(object)(declare (ignore object))∅) (be t))

      (¬ (unmount a))
      (equal (unmount b) {5 2 3})
      (¬ (unmount c))
      (equal (unmount d) {5 2 3})

      (cue-leftmost a (be ∅) (be t)) 
      (cue-leftmost b (be t) (be ∅))
      (cue-leftmost c (be ∅) (be t))
      (cue-leftmost d (be t) (be ∅))

      (r a (λ(object)(declare (ignore object))∅) (be t))
      (r b (λ(object)(= object 5)) (be ∅))
      (r c (λ(object)(declare (ignore object))∅) (be t))
      (r d (λ(object)(= object 5)) (be ∅))

      (eq (state a) void)
      (eq (state b) active)
      (eq (state c) void)
      (eq (state d) active)
      )))

(test-hook test-state-0)


(defun test-state-1 ()
  (let*(
         (a (mount {7 6}))
         )
    (∧
      (= (d◧ a) 7)
      (eq (state a) active)
      (= (d◧ a) 6)
      (eq (state a) void)
      (a a 3)
      (eq (state a) parked)
      (s a)
      (= (r a) 3)
      (eq (state a) active)
      )))
(test-hook test-state-1)

