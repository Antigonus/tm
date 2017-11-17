#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Accepts a list of lists.

Each sublist starts with a command, typically:
  'extract-append
  'copy-append
  'delete

Extract either removes the region from the source, and inserts into the destination, or,
alternatively, it copies the region to the destination, then deletes the regions from the
source.

Command arguments follow the command symbol.  Typicall an argument is a region.

Splice command phases for tm-ref-array-realloc
  1. commands are queued. Extract commands are changed to a copy-delete command pair, with 
     the deletes being queued at the end.
  2. commands are analyzed and destination arrays of appropriate size are created
  3. commands are executed, causing data to be moved and entangled addresses to be adjusted
  4. the source array is deallocated.  The desitnation array becomes the new source array.

All the addresses given in commands during the queing phase are relative to the unmodified
source array.

For now lets leave extract out.  Lets say that splice only has topological operations on 
one tape. We can copy from another tape, but otherwise all the commands affect the specified
tape.

(mk 'tm-ref-realloc
  (make-splice-prog
    {
      {'copy r1}
      {'copy (address-range tm0 0 9)}
      {'copy (mk 'region-ref-array-realloc tm2 {:rightmost-address 12})}
      }))


|#

(deftype splice-prog ()
  (
    queue
    ))

(defun make-splice-prog (splice-prog)
  (let(
        (a-splice-prog (make-instance splice-prog))
        )
    (setf (queue a-splice-prog) splice-prog)
    ))


(defun splice-prog-dest-length (splice-prog)
  (let(
        (chs (make-hash))
        (queue (queue splice-prog))
        new-region
        chasis
        bucket
        (length 0)
        )
    (if (¬ queue)
      length
      (progn 

        ;; shuffle regions by the chasis they belong to
        (⟳(λ(➜next-region)
            (setf new-region (cadar queue)) ; { {'copy region} ...
            (setf chasis (chasis new-region))
            (setf bucket (get-hash chasis chs))
            (if (¬ bucket)
              (setf (get-hash chasis chs) (cons new-region ∅))

              ;; insertion sort by leftbound address into bucket list
              (if (< (@ new-region) (@ (car bucket)))
                (setf (get-hash chasis chs) (cons new-region bucket))
                (when (cdr bucket)
                  (⟳(λ(➜next-bucket)
                      (setf bucketed-region (cadr bucket))
                      (if (< (@ new-region) (@ bucketed-region))
                        (replacd buckets (cons new-region (cdr bucket)))
                        (when (cdr bucket)
                          (setf bucket (cdr bucket))
                          [➜next-bucket]
                          )))))
                ))
            (when (cdr queue)
              (setf queue (cdr queue))
              [➜next-region]
              )))
        
        ;; 
        
                                   )

(defun-typed init ((tm-destination tm-ref-array-realloc) (splice-prog splice-prog) &optional ➜)
  (destructuring-bind
    (
      &key
      (➜ok #'echo)
      (➜bad-init (λ()(error 'bad-init-value)))
      &allow-other-keys
      )
    ➜
   

  )






