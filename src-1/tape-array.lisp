#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A tape-array.

This design is intended to minimize required storage.

I need to change the array reference when expanding the tape, so I used macros on the
interface.  Consequently we do not have dispatch based on argument type. Instead we have
etypecase. This approache leads to a lot of 'unused code being elminated' notes.

At its smallest a tape-array has type 'null (empty) and no instance.  After a single value
is added to a tape array, the tape array acts just like a single value. Etc.

Common Lisp knows the length of an array, and that information must be stored somewhere,
so I don't use an array for short tapes to avoid this overhead. .. Hopefully the compiler
implements structs efficiently, or this approach will be moot.  2nd edition of Asimow's 
book .. Zero, One, Two, Arbitrary ;-)

Cells with index larger than the largest index for the tape array, and those that hold
an empty type instance, are considered to be empty. Empty is not an instance, Hence we
do not let empty type instances leave the tape-array as read values, but instead invoke
empty continutations.

Turing Machine tapes are infinite.  Our tape machine tapes are finite and arbitrarily
expandable.  We showed that for computation that this is sufficient.  Hence, appending to
lengthen a tape is not a topology modifying operation on a Turing Machine; though it is on
our Tape Machine.  It is intresting that the topology modification operation of expension,
the one such operation that is in tape machines but not Turing machines,is safe relative
safe even when machines are entangled.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
  ;; suffix on the struct names is the max-address for a corresponding array
  (defstruct tape-array-max-0 zero)
  (defstruct tape-array-max-1 zero one)
  (defstruct tape-array-max-2 zero one two)
  (defstruct tape-array-max-n array)


;;--------------------------------------------------------------------------------
;; tape properties
;;
  (def-function-class max-active<tape-array> (x &optional ➜))
  (defun-typed max-active<tape-array> ((x null) &optional ➜)
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed max-active<tape-array> ((x tape-array-max-0) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      (if
        (typep (tape-array-max-0-zero x) 'null)
        [➜empty]
        [➜ok 0]
        )))
  (defun-typed max-active<tape-array> ((x tape-array-max-1) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      (cond
        ((∧ 
           (typep (tape-array-max-1-one x) 'null)
           (typep (tape-array-max-1-zero x) 'null)
           )
          [➜empty]
          )
        ((typep (tape-array-max-1-one x) 'null)
          [➜ok 0]
          )
        (t
          [➜ok 1]
          ))))
  (defun-typed max-active<tape-array> ((x tape-array-max-2) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      (cond
        ((∧ 
           (typep (tape-array-max-2-two x) 'null)
           (typep (tape-array-max-2-one x) 'null)
           (typep (tape-array-max-2-zero x) 'null)
           )
          [➜empty]
          )
        ((∧ 
           (typep (tape-array-max-2-two x) 'null)
           (typep (tape-array-max-2-one x) 'null)
           )
          [➜ok 0]
          )
        ((typep (tape-array-max-2-two x) 'null)
          [➜ok 1]
          )
        (t
          [➜ok 2]
          ))))
  (defun-typed max-active<tape-array> ((x tape-array-max-n) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      (let(
            (tape (tape-array-max-n-array x))
            )
        (if
          (= 0 (length tape))
          [➜empty]
          (let(
                (max (1- (length tape)))
                )
            (⟳(λ(➜again)
                (if 
                  (typep (aref tape max) 'null)
                  (if
                    (= max 0)
                    [➜empty]
                    (progn
                      (decf max)
                      [➜again]
                      ))
                  [➜ok max]
                  ))))))))

  (def-function-class max<tape-array> (x &optional ➜))
  (defun-typed max<tape-array> ((x null) &optional ➜)
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed max<tape-array> ((x tape-array-max-0) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok 0]
      ))
  (defun-typed max<tape-array> ((x tape-array-max-1) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok 1]
      ))
  (defun-typed max<tape-array> ((x tape-array-max-2) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok 2]
      ))
  (defun-typed max<tape-array> ((x tape-array-max-n) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (1- (length (tape-array-max-n-array x)))]
      ))


;;--------------------------------------------------------------------------------
;; cell access
;;
  (defmacro r<tape-array>-ret (x ➜ok ➜empty)
    `(if 
      (typep ,x 'null)
      [,➜empty]
      [,➜ok ,x]
    ))

  (defun r<tape-array> (tape-array &optional ➜)
    (destructuring-bind
       (&key
         (address 0)
         (➜ok #'echo)
         (➜empty #'accessed-empty)
         &allow-other-keys
         )
       ➜
       (etypecase tape-array
         (null [➜empty])
         (tape-array-max-0
           (cond
             ((= address 0) (r<tape-array>-ret (tape-array-max-0-zero tape-array) ➜ok ➜empty))
             (t [➜empty])
             ))
         (tape-array-max-1
           (cond
             ((= address 0) (r<tape-array>-ret (tape-array-max-1-zero tape-array) ➜ok ➜empty))
             ((= address 1) (r<tape-array>-ret (tape-array-max-1-one tape-array) ➜ok ➜empty))
             (t [➜empty])
             ))
         (tape-array-max-2
           (cond
             ((= address 0) (r<tape-array>-ret (tape-array-max-2-zero tape-array) ➜ok ➜empty))
             ((= address 1) (r<tape-array>-ret (tape-array-max-2-one tape-array) ➜ok ➜empty))
             ((= address 2) (r<tape-array>-ret (tape-array-max-2-two tape-array) ➜ok ➜empty))
             (t [➜empty])
             ))
         (tape-array-max-n
           (if
             (≥ address (length (tape-array-max-n-array tape-array)))
             [➜empty]
             (r<tape-array>-ret (aref (tape-array-max-n-array tape-array) address) ➜ok ➜empty)
             ))
         )))

  ;; If a programmer wants an implemented empty tail, write an empty type instance
  ;; at the end of the desired empty tail.
  (defmacro w<tape-array> (tape-array instance &optional ➜)
    `(destructuring-bind
       (&key
         (address 0)
         (➜ok (be t))
         (➜alloc-fail #'alloc-fail)
         &allow-other-keys
         )
       ,➜
       (declare (ignore ➜alloc-fail)) ; someday will have to fill this in ...
       (etypecase ,tape-array

         (null
           (cond
             ((= address 0)
               (setf ,tape-array
                 (make-tape-array-max-0
                   :zero ,instance
                   )))
             ((= address 1)
               (setf ,tape-array
                 (make-tape-array-max-1
                   :zero ∅
                   :one ,instance
                   )))
             ((= address 2)
               (setf ,tape-array
                 (make-tape-array-max-2
                   :zero ∅
                   :one ∅
                   :two ,instance
                   )))
             ((≥ address 3)
               (let(
                     (new-array (make-array (1+ address) :initial-element ∅))
                     )
                 (setf (aref new-array address) ,instance)
                 (setf ,tape-array 
                   (make-tape-array-max-n :array new-array)
                   )))
             ))

         (tape-array-max-0
           (cond

             ((= address 0)
               (setf (tape-array-max-0-zero ,tape-array) ,instance)
               )

             ((= address 1)
               (let(
                     (new-tape-array
                       (make-tape-array-max-1
                         :zero (tape-array-max-0-zero ,tape-array)
                         :one ,instance
                         ))
                     )
                 (setf ,tape-array new-tape-array)
                 ))

             ((= address 2)
               (let(
                     (new-tape-array
                       (make-tape-array-max-2
                         :zero (tape-array-max-0-zero ,tape-array)
                         :one ∅
                         :two ,instance
                         ))
                     )
                 (setf ,tape-array new-tape-array)
                 ))

             ((≥ address 3)
               (let(
                     (new-array (make-array (1+ address) :initial-element ∅))
                     )
                 (setf (aref new-array 0) (tape-array-max-0-zero ,tape-array))
                 (setf (aref new-array address) ,instance)
                 (setf ,tape-array 
                   (make-tape-array-max-n :array new-array)
                   )))
             ))

         (tape-array-max-1
           (cond

             ((= address 0)
               (setf (tape-array-max-1-zero ,tape-array) ,instance)
               )

             ((= address 1)
               (setf (tape-array-max-1-one ,tape-array) ,instance)
               )

             ((= address 2)
               (let(
                     (new-tape-array
                       (make-tape-array-max-2
                         :zero (tape-array-max-1-zero ,tape-array)
                         :one  (tape-array-max-1-one ,tape-array)
                         :two ,instance
                         ))
                     )
                 (setf ,tape-array new-tape-array)
                 ))

             ((≥ address 3)
               (let(
                     (new-array (make-array (1+ address) :initial-element ∅))
                     )
                 (setf (aref new-array 0) (tape-array-max-1-zero ,tape-array))
                 (setf (aref new-array 1) (tape-array-max-1-one ,tape-array))
                 (setf (aref new-array address) ,instance)
                 (setf ,tape-array 
                   (make-tape-array-max-n :array new-array)
                   )))
               ))
           
         (tape-array-max-2
           (cond

             ((= address 0)
               (setf (tape-array-max-2-zero ,tape-array) ,instance)
               )

             ((= address 1)
               (setf (tape-array-max-2-one ,tape-array) ,instance)
               )

             ((= address 2)
               (setf (tape-array-max-2-two ,tape-array) ,instance)
               )

             ((≥ address 3)
               (let(
                     (new-array (make-array (1+ address) :initial-element ∅))
                     )
                 (setf (aref new-array 0) (tape-array-max-2-zero ,tape-array))
                 (setf (aref new-array 1) (tape-array-max-2-one ,tape-array))
                 (setf (aref new-array 2) (tape-array-max-2-two ,tape-array))
                 (setf (aref new-array address) ,instance)
                 (setf ,tape-array 
                   (make-tape-array-max-n :array new-array)
                   )))
               ))

         (tape-array-max-n
           (let(
                 (max (max<tape-array> ,tape-array))
                 (array (tape-array-max-n-array ,tape-array))
                 )
             (when
               (> address max)
               (let(
                     (new-array (make-array (1+ address)))
                     (bound (1- address))
                     (i 0)
                     ) 
                 (⟳(λ(➜again)
                     (setf (aref new-array i) (aref array i))  
                     (unless (= i max)
                       (incf i)
                       [➜again]
                       )))
                 (⟳(λ(➜again)
                     (unless (= i bound)
                       (incf i)
                       (setf (aref new-array i) ∅)
                       [➜again]
                       )))
                 (setf (tape-array-max-n-array ,tape-array) new-array)
                 ))
             (setf (aref (tape-array-max-n-array ,tape-array) address) ,instance)
             ))
         )
       [➜ok]
       ))

  (defmacro a◨<tape-array> (tape-array instance &optional ➜)
    `(destructuring-bind
       (&key
         (right-bound (max<tape-array> ,tape-array {:➜ok (λ(max)(1+ max)) :➜empty (λ()0)}))
         &allow-other-keys
         )
       ,➜
       (w<tape-array> ,tape-array ,instance {:address right-bound (o ,➜)})
       ))
