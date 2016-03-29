;;;; -*- Mode: Lisp -*-

;;;; DOC-BITS DB File.

;;;; The actual documentation strings can be modifed.


#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME (SETF COMMON-LISP::TRANSFORM-READ)
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Setter for the slot READ of an object of type TRANSFORM.

Arguments and Values:

OBJECT : a TRANSFORM
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-transform.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::V
                 |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME COMMON-LISP::TRANSFORM-READ
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Accessor for the slot READ of an object of type TRANSFORM.

Arguments and Values:

OBJECT : a TRANSFORM
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-transform.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME (SETF COMMON-LISP::TRANSFORM-WRITE)
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Setter for the slot WRITE of an object of type TRANSFORM.

Arguments and Values:

OBJECT : a TRANSFORM
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-transform.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::V
                 |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME COMMON-LISP::TRANSFORM-WRITE
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Accessor for the slot WRITE of an object of type TRANSFORM.

Arguments and Values:

OBJECT : a TRANSFORM
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-transform.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME (SETF TM::TRANSFORM-TM)
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Setter for the slot TM of an object of type TRANSFORM.

Arguments and Values:

OBJECT : a TRANSFORM
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-transform.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::V
                 |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME TM::TRANSFORM-TM
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Accessor for the slot TM of an object of type TRANSFORM.

Arguments and Values:

OBJECT : a TRANSFORM
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-transform.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME TM::MAKE-TRANSFORM
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "A constructor for the structure TRANSFORM."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-transform.lisp"
   :LAMBDA-LIST (&KEY READ WRITE TM::TM)
   :TYPE-DECLARATIONS ((TYPE T READ) (TYPE T WRITE) (TYPE T TM::TM))
   :FTYPE-DECLARATIONS NIL
   :VALUES (TM::TRANSFORM))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME SI
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "If either object is a tm, or #'tm-mk succeeds on the object, steps in.
     Otherwise cont-tm-mk-fail.
     "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-subspace.lisp"
   :LAMBDA-LIST (TM::TM &OPTIONAL (TM::CONT-OK (BE T))
                 (TM::CONT-TM-MK-FAIL (BE ∅)))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME AI
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Head is on a given cell.  That cell has an object.  The object should be either
     tm-mk-able or be ∅.  If it is ∅, we exit with cont-mk-fail where the programmer
     can then build the singleton sublist of the desired type.  If it is tm-mk-able
     then a new cell is prepended and initialized to object.
     "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-subspace.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT &OPTIONAL (TM::CONT-OK (BE T))
                 (TM::CONT-TM-MK-FAIL (BE ∅))
                 (TM::CONT-NO-ALLOC (Λ NIL (ERROR 'TM::TM-ALLOC-FAIL))))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME AIS
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "like ai, but the tape-machine is stepped into the new cell"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-subspace.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT &OPTIONAL (TM::CONT-OK (BE T))
                 (TM::CONT-TM-MK-FAIL (BE ∅))
                 (TM::CONT-NO-ALLOC (Λ NIL (ERROR 'TM::TM-ALLOC-FAIL))))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME DI
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "(r tm) is an object.  This object should be a tape machine, or an object that can
       be passed to tm-mk to get a tape machine.  This function deallocates the leftmost
       cell from that machine's tape. The deallocated cell is #'a onto spill. Should the
       user attempt to deallocate the last cell of the tape machine, then this routine
       exits via cont-rightmost, where the user can (w tm) replace the object with
       whatever empty marker is used, typically ∅.
       "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-subspace.lisp"
   :LAMBDA-LIST (TM::TM &OPTIONAL TM::SPILL TM::CONT-OK TM::CONT-RIGHTMOST
                 TM::CONT-TM-MK-FAIL)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME ⟳
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "⟳ (pronounced \"do\") accepts a work function.  This work function is to take a
     single step, whatever such a step may be.  The work function accepts two
     continuations.  Typically these are called 'cont-loop', and 'cont-return'.  When the
     work function continues with cont-loop, it is immediately called again.  When it
     continues with cont-return, ⟳ returns.
     "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::WORK)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME ⟳-WORK-STEP
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "⟳-work-step (pronounced \"do work step\") accepts a tape machine, a function to do
     work, and a step function.  The step function must accept as arguments the tape
     machine, and two continuations.  Typically the continuations are called 'cont-loop' and
     'cont-return'.  For example #'s can be used for stepping.  First the
     work function is called, then the step function is called. If the step function
     continues with cont-loop, ⟳-step repeats.  If it continues with continue-return
     ⟳-step returns.
     "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM &OPTIONAL (TM::WORK #'DO-NOTHING) (STEP #'S))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME ∃
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "When returning true, tm head is on the first cell that has an object where pred is true.
    When returning false, tm head is on rightmost, and there was no cell where pred was true."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM TM::PRED &OPTIONAL (TM::CONT-TRUE (BE T))
                 (TM::CONT-FALSE (BE ∅)))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME ¬∀
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "When true, all objects do not match pred, and tm is on the first mismatch."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM TM::PRED &OPTIONAL (TM::CONT-TRUE (BE T))
                 (TM::CONT-FALSE (BE ∅)))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME ¬∃
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "When true, there does not exist an object where pred holds, and tm is at rightmost.
    When false, tm is on an cell with an object where pred holds."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM TM::PRED &OPTIONAL (TM::CONT-TRUE (BE T))
                 (TM::CONT-FALSE (BE ∅)))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME ∀
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "When true all objects match pred.  When false tm will be on the first mismatch."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM TM::PRED &OPTIONAL (TM::CONT-TRUE (BE T))
                 (TM::CONT-FALSE (BE ∅)))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME ∃*
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "like ∃, but all objects will be visited"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM TM::PRED &OPTIONAL (TM::CONT-TRUE (BE T))
                 (TM::CONT-FALSE (BE ∅)))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME S-TOGETHER
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "s-together is passed a list of tape machines.
    Each time s-together is called, all machines in the list are stepped.
    If any of the machines can not be stepped, then none of the machines are stepped.
    cont-exists-end is called with an iterator on the first of the tms that could not
    be stepped.
    "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TMS &OPTIONAL (TM::CONT-OK #'ECHO)
                 (TM::CONT-EXISTS-ON-RIGHTMOST (BE ∅)))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME S*
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "This is a synonym for cue-to-rightmost. There is no guarantee that intermediate
       cells will be visited."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME -S*
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "This is a synonym for cue-to-leftmost. There is no guarantee that intermediate
       cells will be visited."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME A*
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Allocates new cells to tm until running out of fill data."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM TM::TM-FILL &OPTIONAL TM::CONT-OK TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME AS*
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Similar to a*, but tm reflects the steps taken."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM TM::TM-FILL &OPTIONAL TM::CONT-OK TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME D*
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Deallocates all cells right of the head up to and including rightmost.
       If spill is not ∅, then the deallocated right hand side is moved to it.
       If spill is not ∅, and cells can not be moved to it, the objects are
       moved to spill via #'a.  It is only in this last case that we might
       end up taking the cont-no-alloc exit.
      "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM &OPTIONAL TM::SPILL TM::CONT-RIGHTMOST
                 TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME SN
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Step count times.  
       When called, cont-rightmost is passed the remaining count."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM TM::N &OPTIONAL TM::CONT-OK TM::CONT-RIGHTMOST)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME AN
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Similar to calling #'a n times on a dup of tm."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM TM::TM-FILL COUNT &OPTIONAL TM::CONT-OK
                 TM::CONT-RIGHTMOST)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME ASN
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Similar to calling #'as n times. fill is tm that provides initialization
       data. tm and fill are both stepped n times."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM TM::TM-FILL TM::N &OPTIONAL TM::CONT-OK
                 TM::CONT-RIGHTMOST)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME DN
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Given a tape machine and a natural number.
      Like repeating d count times, but specialized versions might be more efficient.
      "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-quantifiers.lisp"
   :LAMBDA-LIST (TM::TM COUNT &OPTIONAL TM::SPILL TM::CONT-OK
                 TM::CONT-RIGHTMOST)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME R
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Given a tape machine, returns the object from the cell under the tape head."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-primitives.lisp"
   :LAMBDA-LIST (TM::TM)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME W
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Writes object into the cell under the tape head."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-primitives.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME CUE-LEFTMOST
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Cue tm's head to the leftmost cell.
       Returns tm.
       This method might not be available for all implementations.
       "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-primitives.lisp"
   :LAMBDA-LIST (TM::TM)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME HEADS-ON-SAME-CELL
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "tm0 and tm1 heads are on the same cell"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-primitives.lisp"
   :LAMBDA-LIST (TM::TM0 TM::TM1 &OPTIONAL TM::CONT-TRUE TM::CONT-FALSE)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME S
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Step tm head to the neighbor cell on the right.
      "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-primitives.lisp"
   :LAMBDA-LIST (TM::TM &OPTIONAL TM::CONT-OK TM::CONT-RIGHTMOST)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME A
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "If no cells are available to be allocated then #'a takes the cont-no-alloc
       continuation.  Otherwise, it allocates a new cell and places it to the right of
       the cell the head is currently on.  The newly allocated cell is initialized with
       the given object.  Allocation failures are quite possible for fixed length
       implementations such as arrays.  The current implementation throws a system
       error if the problem is that the system ran out of memory.
       "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-primitives.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT &OPTIONAL TM::CONT-OK TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME D
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Deallocates one cell to the right of the head.
       If spill exists, #'d tries to put the deallocated cell on spill.
       If spill can not take such cells, then it calls (as spill (r tm)),
       to move the object of the deallocated cell. Cont-ok is called with 
       the object from the deallocated cell.
       "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-primitives.lisp"
   :LAMBDA-LIST (TM::TM &OPTIONAL TM::SPILL TM::CONT-OK TM::CONT-RIGHTMOST
                 TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME (SETF LINE-INFIMUM)
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Setter for the slot INFIMUM of an object of type LINE.

Arguments and Values:

OBJECT : a LINE
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-line.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::V
                 |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME LINE-INFIMUM
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Accessor for the slot INFIMUM of an object of type LINE.

Arguments and Values:

OBJECT : a LINE
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-line.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME (SETF LINE-BOUND)
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Setter for the slot BOUND of an object of type LINE.

Arguments and Values:

OBJECT : a LINE
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-line.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::V
                 |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME LINE-BOUND
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Accessor for the slot BOUND of an object of type LINE.

Arguments and Values:

OBJECT : a LINE
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-line.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME (SETF TM::LINE-Δ)
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Setter for the slot Δ of an object of type LINE.

Arguments and Values:

OBJECT : a LINE
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-line.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::V
                 |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME TM::LINE-Δ
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Accessor for the slot Δ of an object of type LINE.

Arguments and Values:

OBJECT : a LINE
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-line.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME MAKE-LINE
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "A constructor for the structure LINE."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-line.lisp"
   :LAMBDA-LIST (&KEY TM::INFIMUM TM::BOUND Δ)
   :TYPE-DECLARATIONS ((TYPE T TM::INFIMUM) (TYPE T TM::BOUND) (TYPE T Δ))
   :FTYPE-DECLARATIONS NIL
   :VALUES (LINE))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME (SETF TM::INTERVAL-LEFTMOST)
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Setter for the slot LEFTMOST of an object of type INTERVAL.

Arguments and Values:

OBJECT : a INTERVAL
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-interval.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::V
                 |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME TM::INTERVAL-LEFTMOST
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Accessor for the slot LEFTMOST of an object of type INTERVAL.

Arguments and Values:

OBJECT : a INTERVAL
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-interval.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME (SETF TM::INTERVAL-RIGHTMOST)
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Setter for the slot RIGHTMOST of an object of type INTERVAL.

Arguments and Values:

OBJECT : a INTERVAL
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-interval.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::V
                 |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME TM::INTERVAL-RIGHTMOST
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Accessor for the slot RIGHTMOST of an object of type INTERVAL.

Arguments and Values:

OBJECT : a INTERVAL
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-interval.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME TM::MAKE-INTERVAL
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "A constructor for the structure INTERVAL."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-interval.lisp"
   :LAMBDA-LIST (&KEY TM::LEFTMOST TM::RIGHTMOST)
   :TYPE-DECLARATIONS ((TYPE T TM::LEFTMOST) (TYPE T TM::RIGHTMOST))
   :FTYPE-DECLARATIONS NIL
   :VALUES (INTERVAL))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME CUE-TO
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Unless tm-cued type is (typep (type-of tm-orig)), tm-cued is changed to the type of
     tm-orig.  tm-cued is either already on the same tape as tm-orig, or it mounts it.
     tm-cued is cued to the same cell that tm-orig's head is on.  Returns tm-cued.
     "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM-CUED TM::TM-ORIG)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME DUP
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Returns a tm with head on the same cell."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM-ORIG)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME WS
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Writes object into the cell under the tape head, and steps tm."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT &OPTIONAL TM::INDEX TM::CONT-OK
                 TM::CONT-RIGHTMOST)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME R-INDEX
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Read as though tm stepped index number of times before the read.
       index defaults to 1."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM TM::INDEX &OPTIONAL TM::CONT-OK
                 TM::CONT-INDEX-BEYOND-RIGHTMOST)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME CUE-RIGHTMOST
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Cue tm's head to the rightmost cell."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME ON-LEFTMOST
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "tm head is on the leftmost cell."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM &OPTIONAL TM::CONT-TRUE TM::CONT-FALSE)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME ON-RIGHTMOST
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "tm head is on the rightmost cell."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM &OPTIONAL TM::CONT-TRUE TM::CONT-FALSE)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME S≠
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "step tm0 unless it is equal to tm1"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM0 TM::TM1 &OPTIONAL TM::CONT-OK TM::CONT-RIGHTMOST
                 TM::CONT-BOUND)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME A◧
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Allocates a new leftmost cell."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT &OPTIONAL TM::CONT-OK TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME AS
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Like #'a, but tm is stepped to the new cell"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT &OPTIONAL TM::CONT-OK TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME AH◨
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "#'a with a contract that the head is on rightmost.
       Some implementatons will be able to specialize this and make it more efficient.
      "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT &OPTIONAL TM::CONT-OK TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME AH◨S
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "#'as with a contract that the head is on rightmost.
       Some implementatons will be able to specialize this and make it more efficient.
      "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT &OPTIONAL TM::CONT-OK TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME -A
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Allocate a new cell to the left of the head."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT &OPTIONAL TM::CONT-OK TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME -A-S
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Allocate a new cell to the left of the head.  Then step left."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM TM::OBJECT &OPTIONAL TM::CONT-OK TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME D◧
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "Similar to #'d but the leftmost cell is deallocated independent of where the head
       is located, unless the leftmost cell is the rightmost cell, in which case
       cont-rightmost is called. If the tape head is on the leftmost cell, it is moved to
       the new leftmost cell.
       "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM &OPTIONAL TM::SPILL TM::CONT-OK TM::CONT-RIGHTMOST
                 TM::CONT-NO-ALLOC)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME M
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "The object in rightmost is returned.
         All other objects on the tape move right one cell.
         Leftmost is written with the provided fill-object. 
         "
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/tm-derived.lisp"
   :LAMBDA-LIST (TM::TM FILL)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME TM::NEWLINE
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "no inputs, outputs a string that is just a newline"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/string.lisp"
   :LAMBDA-LIST NIL
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME TM::DROP-ENDS
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "input is a sequence, output is a sequence missing the first and last member"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/string.lisp"
   :LAMBDA-LIST (S)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME ON+1
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "tm head is one to the right of the leftmost cell, i.e. on cell 1."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/location.lisp"
   :LAMBDA-LIST (TM::TM &OPTIONAL TM::CONT-TRUE TM::CONT-FALSE)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME ON+N
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "tm head is on cell n."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/location.lisp"
   :LAMBDA-LIST (TM::TM TM::N &OPTIONAL TM::CONT-TRUE TM::CONT-FALSE)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::GENERIC-FUNCTION-DOC-BIT
   :NAME ADDRESS
   :KIND FUNCTION
   :KIND-TAG "Generic Function"
   :DOC-STRING "address of head.  Address is zero when the head is on leftmost."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/location.lisp"
   :LAMBDA-LIST (TM::TM)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL
   :METHODS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME STACK-ENQUEUE
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Pushes an object on to the stack"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/buffers.lisp"
   :LAMBDA-LIST (TM::TM-H◧ TM::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME STACK-DEQUEUE
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Pulls an object off of the stack"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-1/buffers.lisp"
   :LAMBDA-LIST (TM::TM-H◧ &OPTIONAL (TM::CONT-OK #'ECHO)
                 (TM::CONT-EMPTY
                  (ERROR 'TM::DEQUEUE-FROM-EMPTY :TEXT "stack is empty")))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::PACKAGE-DOC-BIT
   :NAME #:TM
   :KIND PACKAGE
   :KIND-TAG "Package"
   :DOC-STRING "The TM Package."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-0/package.lisp"
   :USE-LIST (COMMON-LISP)
   :NICKNAMES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME UNWRAP
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Given a list, returns a new list with one less level of parens per constiuent list object."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-0/list-L.lisp"
   :LAMBDA-LIST (L)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::MACRO-DOC-BIT
   :NAME DEFSYNONYM
   :KIND |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::MACRO
   :KIND-TAG "Macro"
   :DOC-STRING "Define OLD-NAME to be equivalent to NEW-NAME when used in the first position of a Lisp form."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-0/fundamental.lisp"
   :LAMBDA-LIST (TM::OLD-NAME TM::NEW-NAME)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME NL
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "prints a new line"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-0/fundamental.lisp"
   :LAMBDA-LIST (&OPTIONAL (STREAM T))
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES NIL)
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME (SETF COMMON-LISP::BOX-READ)
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Setter for the slot READ of an object of type BOX.

Arguments and Values:

OBJECT : a BOX
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-0/functions.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::V
                 |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME COMMON-LISP::BOX-READ
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Accessor for the slot READ of an object of type BOX.

Arguments and Values:

OBJECT : a BOX
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-0/functions.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME (SETF COMMON-LISP::BOX-WRITE)
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Setter for the slot WRITE of an object of type BOX.

Arguments and Values:

OBJECT : a BOX
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-0/functions.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::V
                 |IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME COMMON-LISP::BOX-WRITE
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "Accessor for the slot WRITE of an object of type BOX.

Arguments and Values:

OBJECT : a BOX
result : a T"
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-0/functions.lisp"
   :LAMBDA-LIST (|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::OBJECT)
   :TYPE-DECLARATIONS NIL
   :FTYPE-DECLARATIONS NIL
   :VALUES (T))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::FUNCTION-DOC-BIT
   :NAME TM::MAKE-BOX
   :KIND FUNCTION
   :KIND-TAG "Function"
   :DOC-STRING "A constructor for the structure BOX."
   :TIMESTAMP 3668149119
   :LOCATION #P"/home/tm/src-0/functions.lisp"
   :LAMBDA-LIST (&KEY READ WRITE)
   :TYPE-DECLARATIONS ((TYPE T READ) (TYPE T WRITE))
   :FTYPE-DECLARATIONS NIL
   :VALUES (BOX))
#S(|IT.UNIMIB.DISCO.MA.CL.HELambdaP|::ASDF-SYSTEM-DOC-BIT
   :NAME #:TM
   :KIND ASDF/SYSTEM:SYSTEM
   :KIND-TAG "System"
   :DOC-STRING "Formalized Iteration Library for Common LISP"
   :TIMESTAMP 3668149118
   :LOCATION #P"/home/tm/tm.asd"
   :DEPENDS-ON ("local-time"))

;;;; end of file -- DOC BITS DB File. --
