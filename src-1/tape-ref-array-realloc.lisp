#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

A tape-ref-array-realloc.

This design is intended to minimize required storage.

I need to change the reference to the array when expanding the tape, so I used macros on
the interface.  Consequently we do not have dispatch based on argument type. Instead we
have etypecase. This approach leads to a lot of 'unused code being elminated' notes when
compiling.

At its smallest a tape-ref-array-realloc has type 'null (empty) and no instance.  After a
single value is added to a tape array, the tape array acts just like a single value. Etc.

Common Lisp knows the length of an array, and that information must be stored somewhere,
so I don't use an array for short tapes to avoid this overhead. .. Hopefully the compiler
implements structs efficiently, or this approach will be moot.  2nd edition of Asimow's 
book .. Zero, One, Two, Arbitrary ;-)

Cells with index larger than the largest index for the tape array, and those that hold
an empty type instance, are considered to be empty. Empty is not an instance, Hence we
do not let empty type instances leave the tape-ref-array-realloc as read values, but instead invoke
empty continutations.

Turing Machine tapes are infinite.  Our tape machine tapes are finite and arbitrarily
expandable.  We showed that for computation that this is sufficient.  Hence, appending to
the active area to lengthen the active area on a tape is not a topology modifying
operation on a Turing Machine; though it is on our Tape Machine.  It is intresting that
the topology modification operation of expension, the one such operation that is in tape
machines but not Turing machines, is relative safe even when machines are entangled.

|# 

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type
;;
;; An empty tape-ref-array-realloc will not typep as tape-ref-array-realloc - it is a feature
;; of Lisp that ∅ is its own type.  Perhaps we should define a empty array type.  A tape array
;; passed into a function can not be made longer, as the base reference is needed in case there
;; is a realloc operation.
;;
  (defstruct tape-ref-array-realloc)
  ;; suffix on the struct names is the max-address for a corresponding array
  (defstruct (tape-ref-array-realloc-max-0 (:include tape-ref-array-realloc)) zero)
  (defstruct (tape-ref-array-realloc-max-1 (:include tape-ref-array-realloc)) zero one)
  (defstruct (tape-ref-array-realloc-max-2 (:include tape-ref-array-realloc)) zero one two)
  (defstruct (tape-ref-array-realloc-max-n (:include tape-ref-array-realloc)) array)

;;--------------------------------------------------------------------------------
;; tape properties
;;
  (def-function-class max-active<tape-ref-array-realloc> (x &optional ➜))
  (defun-typed max-active<tape-ref-array-realloc> ((x null) &optional ➜)
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed max-active<tape-ref-array-realloc> ((x tape-ref-array-realloc-max-0) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      (if
        (typep (tape-ref-array-realloc-max-0-zero x) 'null)
        [➜empty]
        [➜ok 0]
        )))
  (defun-typed max-active<tape-ref-array-realloc> ((x tape-ref-array-realloc-max-1) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      (cond
        ((∧ 
           (typep (tape-ref-array-realloc-max-1-one x) 'null)
           (typep (tape-ref-array-realloc-max-1-zero x) 'null)
           )
          [➜empty]
          )
        ((typep (tape-ref-array-realloc-max-1-one x) 'null)
          [➜ok 0]
          )
        (t
          [➜ok 1]
          ))))
  (defun-typed max-active<tape-ref-array-realloc> ((x tape-ref-array-realloc-max-2) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      (cond
        ((∧ 
           (typep (tape-ref-array-realloc-max-2-two x) 'null)
           (typep (tape-ref-array-realloc-max-2-one x) 'null)
           (typep (tape-ref-array-realloc-max-2-zero x) 'null)
           )
          [➜empty]
          )
        ((∧ 
           (typep (tape-ref-array-realloc-max-2-two x) 'null)
           (typep (tape-ref-array-realloc-max-2-one x) 'null)
           )
          [➜ok 0]
          )
        ((typep (tape-ref-array-realloc-max-2-two x) 'null)
          [➜ok 1]
          )
        (t
          [➜ok 2]
          ))))
  (defun-typed max-active<tape-ref-array-realloc> ((x tape-ref-array-realloc-max-n) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      (let(
            (tape (tape-ref-array-realloc-max-n-array x))
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

  (def-function-class max<tape-ref-array-realloc> (x &optional ➜))
  (defun-typed max<tape-ref-array-realloc> ((x null) &optional ➜)
    (destructuring-bind
      (&key
        (➜empty #'accessed-empty)
        &allow-other-keys
        )
      ➜
      [➜empty]
      ))
  (defun-typed max<tape-ref-array-realloc> ((x tape-ref-array-realloc-max-0) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok 0]
      ))
  (defun-typed max<tape-ref-array-realloc> ((x tape-ref-array-realloc-max-1) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok 1]
      ))
  (defun-typed max<tape-ref-array-realloc> ((x tape-ref-array-realloc-max-2) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok 2]
      ))
  (defun-typed max<tape-ref-array-realloc> ((x tape-ref-array-realloc-max-n) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (1- (length (tape-ref-array-realloc-max-n-array x)))]
      ))


;;--------------------------------------------------------------------------------
;; cell access
;;
  (defmacro r<tape-ref-array-realloc>-ret (x ➜ok ➜empty)
    `(if 
      (typep ,x 'null)
      [,➜empty]
      [,➜ok ,x]
    ))

  (defun r<tape-ref-array-realloc> (tape &optional ➜)
    (destructuring-bind
       (&key
         (address 0)
         (➜ok #'echo)
         (➜empty #'accessed-empty)
         &allow-other-keys
         )
       ➜
       (etypecase tape
         (null [➜empty])
         (tape-ref-array-realloc-max-0
           (cond
             ((= address 0) (r<tape-ref-array-realloc>-ret (tape-ref-array-realloc-max-0-zero tape) ➜ok ➜empty))
             (t [➜empty])
             ))
         (tape-ref-array-realloc-max-1
           (cond
             ((= address 0) (r<tape-ref-array-realloc>-ret (tape-ref-array-realloc-max-1-zero tape) ➜ok ➜empty))
             ((= address 1) (r<tape-ref-array-realloc>-ret (tape-ref-array-realloc-max-1-one tape) ➜ok ➜empty))
             (t [➜empty])
             ))
         (tape-ref-array-realloc-max-2
           (cond
             ((= address 0) (r<tape-ref-array-realloc>-ret (tape-ref-array-realloc-max-2-zero tape) ➜ok ➜empty))
             ((= address 1) (r<tape-ref-array-realloc>-ret (tape-ref-array-realloc-max-2-one tape) ➜ok ➜empty))
             ((= address 2) (r<tape-ref-array-realloc>-ret (tape-ref-array-realloc-max-2-two tape) ➜ok ➜empty))
             (t [➜empty])
             ))
         (tape-ref-array-realloc-max-n
           (if
             (≥ address (length (tape-ref-array-realloc-max-n-array tape)))
             [➜empty]
             (r<tape-ref-array-realloc>-ret (aref (tape-ref-array-realloc-max-n-array tape) address) ➜ok ➜empty)
             ))
         )))

  ;; If a programmer wants an implemented empty tail, write an empty type instance
  ;; at the end of the desired empty tail.
  (defmacro w<tape-ref-array-realloc> (tape-ref-array-realloc instance &optional ➜)
    `(destructuring-bind
       (&key
         (address 0)
         (➜ok (be t))
         (➜alloc-fail #'alloc-fail)
         &allow-other-keys
         )
       ,➜
       (declare (ignore ➜alloc-fail)) ; someday will have to fill this in ...
       (etypecase ,tape-ref-array-realloc

         (null
           (cond
             ((= address 0)
               (setf ,tape-ref-array-realloc
                 (make-tape-ref-array-realloc-max-0
                   :zero ,instance
                   )))
             ((= address 1)
               (setf ,tape-ref-array-realloc
                 (make-tape-ref-array-realloc-max-1
                   :zero ∅
                   :one ,instance
                   )))
             ((= address 2)
               (setf ,tape-ref-array-realloc
                 (make-tape-ref-array-realloc-max-2
                   :zero ∅
                   :one ∅
                   :two ,instance
                   )))
             ((≥ address 3)
               (let(
                     (new-array (make-array (1+ address) :initial-element ∅))
                     )
                 (setf (aref new-array address) ,instance)
                 (setf ,tape-ref-array-realloc 
                   (make-tape-ref-array-realloc-max-n :array new-array)
                   )))
             ))

         (tape-ref-array-realloc-max-0
           (cond

             ((= address 0)
               (setf (tape-ref-array-realloc-max-0-zero ,tape-ref-array-realloc) ,instance)
               )

             ((= address 1)
               (let(
                     (new-tape-ref-array-realloc
                       (make-tape-ref-array-realloc-max-1
                         :zero (tape-ref-array-realloc-max-0-zero ,tape-ref-array-realloc)
                         :one ,instance
                         ))
                     )
                 (setf ,tape-ref-array-realloc new-tape-ref-array-realloc)
                 ))

             ((= address 2)
               (let(
                     (new-tape-ref-array-realloc
                       (make-tape-ref-array-realloc-max-2
                         :zero (tape-ref-array-realloc-max-0-zero ,tape-ref-array-realloc)
                         :one ∅
                         :two ,instance
                         ))
                     )
                 (setf ,tape-ref-array-realloc new-tape-ref-array-realloc)
                 ))

             ((≥ address 3)
               (let(
                     (new-array (make-array (1+ address) :initial-element ∅))
                     )
                 (setf (aref new-array 0) (tape-ref-array-realloc-max-0-zero ,tape-ref-array-realloc))
                 (setf (aref new-array address) ,instance)
                 (setf ,tape-ref-array-realloc 
                   (make-tape-ref-array-realloc-max-n :array new-array)
                   )))
             ))

         (tape-ref-array-realloc-max-1
           (cond

             ((= address 0)
               (setf (tape-ref-array-realloc-max-1-zero ,tape-ref-array-realloc) ,instance)
               )

             ((= address 1)
               (setf (tape-ref-array-realloc-max-1-one ,tape-ref-array-realloc) ,instance)
               )

             ((= address 2)
               (let(
                     (new-tape-ref-array-realloc
                       (make-tape-ref-array-realloc-max-2
                         :zero (tape-ref-array-realloc-max-1-zero ,tape-ref-array-realloc)
                         :one  (tape-ref-array-realloc-max-1-one ,tape-ref-array-realloc)
                         :two ,instance
                         ))
                     )
                 (setf ,tape-ref-array-realloc new-tape-ref-array-realloc)
                 ))

             ((≥ address 3)
               (let(
                     (new-array (make-array (1+ address) :initial-element ∅))
                     )
                 (setf (aref new-array 0) (tape-ref-array-realloc-max-1-zero ,tape-ref-array-realloc))
                 (setf (aref new-array 1) (tape-ref-array-realloc-max-1-one ,tape-ref-array-realloc))
                 (setf (aref new-array address) ,instance)
                 (setf ,tape-ref-array-realloc 
                   (make-tape-ref-array-realloc-max-n :array new-array)
                   )))
               ))
           
         (tape-ref-array-realloc-max-2
           (cond

             ((= address 0)
               (setf (tape-ref-array-realloc-max-2-zero ,tape-ref-array-realloc) ,instance)
               )

             ((= address 1)
               (setf (tape-ref-array-realloc-max-2-one ,tape-ref-array-realloc) ,instance)
               )

             ((= address 2)
               (setf (tape-ref-array-realloc-max-2-two ,tape-ref-array-realloc) ,instance)
               )

             ((≥ address 3)
               (let(
                     (new-array (make-array (1+ address) :initial-element ∅))
                     )
                 (setf (aref new-array 0) (tape-ref-array-realloc-max-2-zero ,tape-ref-array-realloc))
                 (setf (aref new-array 1) (tape-ref-array-realloc-max-2-one ,tape-ref-array-realloc))
                 (setf (aref new-array 2) (tape-ref-array-realloc-max-2-two ,tape-ref-array-realloc))
                 (setf (aref new-array address) ,instance)
                 (setf ,tape-ref-array-realloc 
                   (make-tape-ref-array-realloc-max-n :array new-array)
                   )))
               ))

         (tape-ref-array-realloc-max-n
           (let(
                 (max (max<tape-ref-array-realloc> ,tape-ref-array-realloc))
                 (array (tape-ref-array-realloc-max-n-array ,tape-ref-array-realloc))
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
                 (setf (tape-ref-array-realloc-max-n-array ,tape-ref-array-realloc) new-array)
                 ))
             (setf (aref (tape-ref-array-realloc-max-n-array ,tape-ref-array-realloc) address) ,instance)
             ))
         )
       [➜ok]
       ))

  (defmacro a◨<tape-ref-array-realloc> (tape instance &optional ➜)
    `(destructuring-bind
       (&key
         (➜ok (be t))
         (➜alloc-fail #'alloc-fail)
         &allow-other-keys
         )
       ,➜
       (declare (ignore ➜alloc-fail)) ; someday will have to fill this in ...
       (max<tape-ref-array-realloc> ,tape
         {
           :➜ok
           (λ(max)
             (w<tape-ref-array-realloc> ,tape ,instance {:address (1+ max) (o ,➜)})
             [➜ok]
             )
           :➜empty
           (λ()
             (w<tape-ref-array-realloc> ,tape ,instance ,➜)
             [➜ok]
             )
           })
       ))

  ;; writes the instance over the first null reference, or appends, then returns the index.
  ;; separate exists for write and append facilitates the caller in keeping track of sparsity of the heap.
  ;; The caller might want to '#'compact a heap that has become too sparse, or has too long of a tail.
  ;;
    (defmacro write-heap<tape-ref-array-realloc> (tape instance &optional ➜)
      `(destructuring-bind
         (&key
           (➜write #echo)
           (➜append #echo)
           (➜alloc-fail #'alloc-fail)
           &allow-other-keys
           )
         ,➜
         (declare (ignore ➜alloc-fail)) ; someday will have to fill this in ...
         (max<tape-ref-array-realloc> ,tape
           {
             :➜empty
             (λ()
               (w<tape-ref-array-realloc> ,tape ,instance ,➜)
               [➜apppend 0]
               )

             :➜ok
             (λ(max)
               (let((i 0))
                 (⟳(λ(➜again) ; search for an empty cell
                     (r<tape-ref-array-realloc> ,tape
                       {
                         :address i
                         :➜empty 
                         (λ() 
                           (w<tape-ref-array-realloc> ,tape ,instance {:address i (o ,➜)})
                           [➜write i]
                           )
                         :➜ok
                         (λ(cell-contents)
                           (declare (ignore cell-contents))
                           (cond
                             ((= i max) ; tape is full, append instance to the end
                               (w<tape-ref-array-realloc> ,tape ,instance {:address (1+ i) (o ,➜)}) ; append
                               [➜append i]
                               )
                             (t
                               (incf i)
                               [➜again] ; keep searching for an empty cell
                               )))
                         })))))
             })))

;; need to add the weak pointer part
    (defmacro write-heap-weak<tape-ref-array-realloc> (tape instance &optional ➜)
      `(destructuring-bind
         (&key
           (➜ok (be t))
           (➜alloc-fail #'alloc-fail)
           &allow-other-keys
           )
         ,➜
         (declare (ignore ➜alloc-fail)) ; someday will have to fill this in ...
         (max<tape-ref-array-realloc> ,tape
           {
             :➜empty
             (λ()
               (w<tape-ref-array-realloc> ,tape ,instance ,➜)
               [➜ok 0]
               )

             :➜ok
             (λ(max)
               (let((i 0))
                 (⟳(λ(➜again) ; search for an empty cell
                     (r<tape-ref-array-realloc> ,tape
                       {
                         :address i
                         :➜empty 
                         (λ() 
                           (w<tape-ref-array-realloc> ,tape ,instance {:address i (o ,➜)})
                           )
                         :➜ok
                         (λ(cell-contents)
                           (declare (ignore cell-contents))
                           (cond
                             ((= i max) ; tape is full, append instance to the end
                               (w<tape-ref-array-realloc> ,tape ,instance {:address (1+ i) (o ,➜)}) ; append
                               )
                             (t
                               (incf i)
                               [➜again] ; keep searching for an empty cell
                               )))
                         })))
                 [➜ok i] ;whether we found an empty cell and wrote it, or append to the end, all is ➜ok
                 ))
             })))
