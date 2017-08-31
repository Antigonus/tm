#|
Copyright (c) 2017 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

Implementation of cell intended for use in a list.

CLOS version of the cons cell.

cell-list contents is a single instance.
cell-list-plex contents is a plex of instances

The neighbors slot is a tape array of neighbor links.  A neighbor link is either simplex
or multiplex.  We distinguish between these two possibilities based on type.

A cell with a subspace is simply a cell with an additional neighbor link than the
two neighbors associated with a tape. Cells might also be used in tree structures,
etc.  there is nothing that interinically limits them to being used in tapes.

In this implementation we are using mutual neighbor links.  If A is a neighbor
of B, then B is a nieghbor of A.

|#

(in-package #:tm)

;;--------------------------------------------------------------------------------
;; type definition
;;

  ;; The contents slot of a cell-list holds the contents directly.  With a cell-list-plex
  ;; the contents slot holds a plex.  The cell contents becomes multiplexed when a write
  ;; occurs to other than channel 0.
  ;;
  ;; The neighbor slot holds a tape array.  If a neighbor array instance is ∅, then
  ;; the cell has no such nth neighbor.  If the neighbor array instance is a cell-list
  ;; then it is a direct link.  If the neighbor array instance is an array, then it is taken
  ;; as a plex.  Each value in the plex is either ∅ or a cell-list direct link.
  ;; 
    (def-type cell-list (cell)
      (
        (contents 
          :accessor contents
          )
        (neighbor
          :initform (make<tape-array>)
          :accessor neighbor
          )
        ))

    (def-type cell-list-plex (cell)()) ; contents taken to be a plex

  (defun-typed init ((cell cell-list) instance &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        ;;(➜fail (λ()(error 'bad-init-value)))
        &allow-other-keys
        )
      ➜
      (setf (contents cell) instance)
      [➜ok cell]
      ))


;;--------------------------------------------------------------------------------
;; read
;;
  (defun-typed r<contents> ((cell cell-list) &optional ➜)
    (destructuring-bind
      (&key
        (➜ok #'echo)
        &allow-other-keys
        )
      ➜
      [➜ok (contents cell)]
      ))

  (defun-typed r<contents> ((cell cell-list-plex) &optional ➜)
    (destructuring-bind
      (&key
        (channel 0)
        parents 
        (➜ok #'echo)
        (➜bad-channel (λ()(error 'bad-channel)))
        &allow-other-keys
        )
      ➜
      (r<plex> (contents cell) channel parents
        {
          :➜ok ➜ok
          :➜bad-channel ➜bad-channel
          })
      ))

  (defun-typed r<neighbor> ((cell cell-list) &optional ➜)
    (destructuring-bind
      (&key
        (address 0) ; which neighbor
        (channel 0)
        parents 
        (➜ok #'echo)
        (➜empty #'accessed-empty)
        (➜bad-channel (λ()(error 'bad-channel)))
        &allow-other-keys
        )
      ➜
      (r<tape-array> (neighbor cell)
        {
          :address address
          :➜ok
          (λ(neighbor)
            (cond
              ((typep neighbor 'empty) [➜empty])
              ((typep neighbor 'cell) [➜ok neighbor])
              (t ; the only possiblility left is that it is a plex
                (r<plex> neighbor channel parents
                  {
                    :➜ok ➜ok
                    :➜bad-channel ➜bad-channel
                    :➜empty ➜empty
                    }))))
          })))


  (defmacro r<cell> (cell accessor ➜)
    `(destructuring-bind
       (&key
         (➜ok #'echo)
         (➜no-universe (λ()(error 'no-universe)))
         (sid 0)
         (channel 0) ; if channel is nonzero, then 'parents' must be supplied, enforced by contract with programmer
         parents 
         &allow-other-keys
         )
       ,➜
       (let(contents)
         (⟳ (λ(➜again)
              (setf contents (r<tape-array> (apply ,accessor ,cell) channel))
              (if
                contents
                [➜ok (r<tape-array> contents sid)]
                (if
                  (= channel 0)
                  [➜no-universe]
                  (progn
                    (setf channel (gethash channel parents))
                    [➜again]
                    ))))))
       ))


  (defun-typed r<contents> ((cell cell-list) &optional ➜)
    (r<cell> cell contents ➜)
    )

  (defun-typed r<neighbor> ((cell cell-list) &optional ➜)
    (r<cell> cell neighbor ➜)
    )

;;--------------------------------------------------------------------------------
;; write
;;

  (defmacro w<cell> (cell instance accessor ➜)
    `(destructuring-bind
       (&key
         (➜ok (be t))
         (address 0)
         &allow-other-keys
         )
       ,➜
       (w<tape-array> (,accessor ,cell) ,instance {:address address})
       [➜ok]
       ))
  (defun-typed w<contents> ((cell cell-list) instance &optional ➜)
    (w<cell> cell instance contents ➜)
    )
  (defun-typed w<neighbor> ((cell cell-list) instance &optional ➜)
    (w<cell> cell instance neighbor ➜)
    )

  (defmacro wa<cell> (cell instance accessor operator ➜)
    `(destructuring-bind
       (&key
         (➜ok #'echo)
         (address 0)
         (channel 0)
         parents
         &allow-other-keys
         )
       ,➜
       (expand<tape-array> (,accessor ,cell) channel)
       (,operator
         (r<tape-array> (,accessor ,cell) {:address channel})
         ,instance
         {:address address}
         )
       [➜ok]
       ))
  (defun-typed w<contents> ((cell cell-list) instance &optional ➜)
    (wa<cell> cell instance contents w<tape-array> ➜)
    )
  (defun-typed w<neighbor> ((cell cell-list) instance &optional ➜)
    (wa<cell> cell instance neighbor w<tape-array> ➜)
    )
  (defun-typed a◨<contents> ((cell cell-list) instance &optional ➜)
    (wa<cell> cell instance contents a◨<tape-array> ➜)
    )
  (defun-typed a◨<neighbor> ((cell cell-list) instance &optional ➜)
    (wa<cell> cell instance neighbor a◨<tape-array> ➜)
    )

