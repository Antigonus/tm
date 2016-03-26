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
               (:module "src-0"
                 :components (
                              (:file "package")

                              (:file "fundamental")
                              (:file "list-L")
                              (:file "functions")
                              ))

               (:module "src-test"
                 :components (
                              (:file "test")
                              ))

               (:module "test-0"
                 :components (
                              (:file "list-L")
                               ))

               (:module "src-1"
                 :components (
                             ;; conditions
                               (:file "conditions")

                             ;; interface definition
                               (:file "tm-mk")
                               (:file "tm-primitives")
                               (:file "tm-derived")
                               (:file "tm-quantifiers")
                               (:file "tm-subspace")
                               (:file "tm-convert")

                             ;; general properties
                               (:file "location")
                               (:file "length")

                             ;; generic adapters
                               (:file "buffers")  ; stack and queue
                               (:file "tm-interval")
                               (:file "tm-transform")

                             ;; interpretting another tm as a tree
                               (:file "tm-depth-mk")
                               (:file "tm-depth-primitives")
                               (:file "tm-depth-convert")

                               (:file "tm-breadth-mk")
                               (:file "tm-breadth-primitives")
                               (:file "tm-breadth-convert")

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
                              ))

               (:module "test-1"
                :components (
                              (:file "tm-derived")
                              (:file "tm-subspace")
                              (:file "tm-quantifiers")

                              (:file "length")
                              (:file "location")

                              (:file "buffers")
                              (:file "tm-interval")
                              (:file "tm-depth")
                              (:file "tm-breadth")

                              (:file "tm-void")
                              (:file "tm-line")

                              (:file "tm-list-mk")
                              (:file "tm-list-primitives")
                              (:file "tm-list-derived")
                              ))

               (:module "src-2"
                :components (
                              (:file "worker")
                              (:file "worker-utilities")

                              (:file "tm-array-adj-mk")
                              (:file "tm-array-adj-primitives")
                              (:file "tm-array-adj-derived")
                              (:file "tm-array-adj-convert")

                              #|
                              (:file "list-lang") ; accessor lang here delta-s etc.
                              |#
                              ))

               (:module "test-2"
                 :components (
                               (:file "worker")
                               (:file "tm-array-adj")
                               ))
             
               ))
  
  



