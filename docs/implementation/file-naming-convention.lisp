file name convention


0. primitive-derived | generic-implementation

   The generic interface will define some generic 'primitives'.  Each implementation
   type (.e.g list, array, etc.) must provide method definitions for all the generic
   primitives.  These implementations are known as the 'primitive functions'.

   Derived functions are said to be generic if they refer only to generic interface
   primitives or to other prior derived functions.  Generic derived functions can not
   refer directly to slots.

   Notice then, that no implementation specific functions (i.e. list, array, etc. specific
   functions) need to be defined for the generic derived functions.  However, sometimes
   for performance reasons an implementation will provide an implementation specific
   derived function.

   If a function refers to fields in the object as defined in the -def file, it is an
   implementation

   A derived function that calls only other derived functions and primitives is generic

1. abreviations

  tm - tape machine

  access types
    ea - entanglement accounting
    nd - non-destructive
    solo - only one head
    ts - threads - multiple threads that are entanglement accounted

2. generic interface files

   for any access type
   tm-(def|derived|mk|primitives ..).lisp  

   specific access types
   (solo|nd|ea|ts)-tm-(def|primitives ..)


3. implementations function files

   for any access type
   (list|array| ...)-tm-(def|mk|primitives)

   specific access types
   (list|array|...)-(solo|nd|ea|ts)-tm-(def|primitives)




