#|

Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#


(in-package :asdf-user)

(defsystem #:tm
  :name "tm"
  :version "0.5"
  :author "Thomas W. Lynch <thomas.lynch@reasoningtechnology.com>"
  :description "Formalized Iteration Library for Common LISP"
  :license "MIT License"
  :depends-on ("local-time")
  :serial t
  :components(
               (:module "package-def"
                 :components (
                               (:file "package")
                               (:file "conditions")
                               ))

               (:module "src-0"
                 :components (
                              (:file "synonyms")
                              (:file "list-qL")
                              (:file "reader-macros")
                              (:file "functions")
                              ))

               (:module "src-test"
                 :components (
                              (:file "test")
                              ))

               (:module "test-0"
                 :components (
                              (:file "list-qL")
                               ))

               (:module "src-1"
                 :components (
                             ;; interface definition
                               (:file "mk")
                               (:file "parking")
                               (:file "tm-primitives")
                               (:file "tm-derived")
                               (:file "tm-quantifiers")
                               (:file "tm-subspace")
                               (:file "tm-convert")

                             ;; general properties
                               (:file "location")
                               (:file "length")

                             ;; trivial machines
                               (:file "tm-void")
                               (:file "tm-parked")

                             ;; transforms
                               (:file "tm-region")
                               (:file "buffers")

                               (:file "tm-depth-mk")
                               (:file "tm-depth-primitives")
                               (:file "tm-depth-convert")

                               (:file "tm-breadth-mk")
                               (:file "tm-breadth-primitives")
                               (:file "tm-breadth-convert")

                             ;; simple generators 
                               (:file "tm-line")

                             ;; list implementation
                               (:file "tm-list-mk")
                               (:file "tm-list-primitives")
                               (:file "tm-list-derived")
                               (:file "tm-list-convert")

                             ;; dataflow
                               (:file "dataflow")
                              ))


               (:module "test-1"
                :components (
                              (:file "tm-void")
;;                              (:file "tm-transitions")
                              
                              (:file "tm-derived")

                              (:file "tm-subspace")
                              (:file "tm-quantifiers")

                              (:file "length")
                              (:file "location")

                              (:file "buffers")
                              (:file "tm-region")
                              (:file "tm-depth")
                              (:file "tm-breadth")

                              (:file "tm-line")

                              (:file "tm-list-mk")
                              (:file "tm-list-primitives")
                              (:file "tm-list-derived")
                              ))

#|


               (:module "src-2"
                :components (

                              (:file "worker")
                              (:file "worker-utilities")

                              (:file "tm-array-adj-mk")
                              (:file "tm-array-adj-primitives")
                              (:file "tm-array-adj-derived")
                              (:file "tm-array-adj-convert")

                              (:file "tm-array-mk")
                              (:file "tm-array-primitives")
                              (:file "tm-array-derived")
                              (:file "tm-array-convert")

                              (:file "tm-aggregate-mk")
                              (:file "tm-aggregate-primitives")

                              (:file "access-lang")
                              ))

               (:module "test-2"
                 :components (
                               (:file "worker")
                               (:file "tm-array-adj")
                               (:file "tm-aggregate")
                               (:file "access-lang")
                               ))
|#
               ))
  
  



