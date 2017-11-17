#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

The head of a tm-ref-array is an address, and integer.  There is no interaction between
the value on the head and the garbage collector.

|#

;;--------------------------------------------------------------------------------
;; type
;;
  ;; here we inherit and add a slot of the right bound
  (def-type region-ref-array-realloc (region tm-ref-array-realloc)
    (rightmost-address :accessor rightmost-address)
    )

  (def-type region-ref-array-realloc-abandoned (region-ref-array-realloc))
  (def-type region-ref-array-realloc-empty (region-ref-array-realloc))
  (def-type region-ref-array-realloc-parked (region-ref-array-realloc))
  (def-type region-ref-array-realloc-active (region-ref-array-realloc))

  (defun-typed to-abandoned ((region-abandoned region-ref-array-realloc))
    (setf (chasis tm) ∅) ; so that a bunch of data doesn't stay alive while the tm is in limbo
    (change-class region 'region-ref-array-realloc-abandoned)
    )
  (defun-typed to-empty     ((region-empty region-ref-array-realloc))
    (setf (head tm) 'enpty) ; to prevent unintended communication on using an empty machine
    (change-class region 'region-ref-array-realloc-empty)
    )
  (defun-typed to-parked    ((region-parked region-ref-array-realloc))
    (setf (head tm) 'parked) ; to prevent unintended communication on a parked  machine
    (change-class region 'region-ref-array-realloc-parked)
    )
  (defun-typed to-active  ((region-active region-ref-array-realloc)) 
    (change-class region 'region-ref-array-realloc-active)
    )

;;--------------------------------------------------------------------------------
;; absolue head control
;;
  (defun-typed u-right ((r region-ref-array-realloc) address &optional ➜)
    (destructuring-bind
      (
        &key
        (address 0) ; for higher rank tapes the cell address will be a list
        (➜ok (be t))
        &allow-other-keys
        )
      ➜
      (setf (rightmost-adddress r) address)
      [➜ok]
      ))


  (defun-typed max@ ((r region-ref-array-realloc) &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (rightmost-address r)] ; tape array head is the location
      ))

;;--------------------------------------------------------------------------------
;; relative head control
;;

;;--------------------------------------------------------------------------------
;; instance creation
;;

  ;; (mk 'region-ref-array-realloc address)
  (defun-typed init ((r region-ref-array-realloc) (tm tm) &optional ➜)
    (destructuring-bind
      (
        &key
        address
        &allow-other-keys
        )
      ➜
      (cond
        ((¬ address) [➜bad-init])
        (t
          (setf (rightmost-address r) address)
          (call-next-method r tm ➜) ; calls the entangle initializer for tm
          ))))

  (defun-typed init ((r1 tm-ref-array-realloc) (r0 tm-ref-array-realloc) &optional ➜)
    (destructuring-bind
      (
        &key
        (address (rightmost-address r0))
        &allow-other-keys
        )
      ➜
    (call-next-method r1 r0 
      {
        :address address
        (o ➜)
        }))

  (def-function-class address-range (tm lefmost-address rightmost-address &optional ➜))
  (defun-typed address-range ((tm tm-ref-array-realloc) lefmost-address rightmost-address &optional ➜)
    (destructuring-bind
      (
        &key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      (mk 'region-ref-array-realloc tm 
        {
          :address rightmost-address
          :➜ok (λ(r)
                 (if (= leftmost-address 0)
                   (to-parked r)
                   (u r {:address (1- leftmost-address)})
                   [➜ok r]
                   ))
          (o ➜)
          })))
