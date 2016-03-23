#|

Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#


(in-package :asdf-user)

(defsystem #:tm
  :name "tm"
  :version "0.2"
  :author "Thomas W. Lynch <thomas.lynch@reasoningtechnology.com>"
  :description "Formalized Iteration Library for Common LISP"
  :depends-on ("local-time")
  :serial t
  :components(
               (:module "src"
                :components (
                              (:file "package")
                              (:file "conditions")

                              (:file "fundamental")
                              (:file "list-L")
                              (:file "functions")

                              ;; interface definition
                              (:file "tm-mk")
                              (:file "tm-primitives")
                              (:file "tm-derived")
                              (:file "tm-subspace")
                              (:file "tm-quantifiers")
                              (:file "tm-convert")

                              ;; general properties
                              (:file "location")
                              (:file "length")

                              ;; generic adapters
                              (:file "buffers")  ; stack and queue
                              (:file "tm-interval")

                              (:file "tm-depth-mk")
                              (:file "tm-depth-primitives")
                              (:file "tm-transform")
                              (:file "tm-depth-convert")

;                              (:file "tm-breadth")

                              ;; simple generators 
                              (:file "tm-void")
                              (:file "tm-singular-affine")
                              (:file "tm-singular-projective")
                              (:file "tm-line")

                              ;; list implementation
                              (:file "tm-list-mk")
                              (:file "tm-list-primitives")
                              (:file "tm-list-derived")
                              (:file "tm-list-buffers")
                              (:file "tm-list-convert")

                              (:file "tm-array-adj-mk")
                              (:file "tm-array-adj-primitives")
                              (:file "tm-array-adj-derived")
                              (:file "tm-array-adj-convert")


                              #|
                              (:file "tm-array")
                              (:file "list-lang") ; accessor lang here delta-s etc.
                              |#
                              ))

               (:module "test-framework"
                :components (
                              (:file "framework")
                              ))

               (:module "test"
                :components (
                              (:file "list-L")

                              (:file "tm-derived")
                              (:file "tm-subspace")
                              (:file "tm-quantifiers")

                              (:file "length")
                              (:file "location")

                              (:file "buffers")
                              (:file "tm-depth")
;;                              (:file "tm-breadth")

                              (:file "tm-void")
                              (:file "tm-line")

                              (:file "tm-list-mk")
                              (:file "tm-list-primitives")
                              (:file "tm-list-derived")

                              (:file "tm-array-adj")
                              ))

               (:module "utilities"
                :components (
                              (:file "worker")
                              (:file "test-worker")
                              (:file "worker-utilities")
                              ))

               ))
  
  



