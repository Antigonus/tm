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
  :depends-on ("local-time" "trivial-garbage")
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
                               (:file "functions")
                               ))

               (:module "src-generic"
                 :components (
                               (:file "tm-def")
                               (:file "tm-mk")
                               (:file "tm-primitives")
                               (:file "tm-derived")
                               (:file "tm-quantifiers")
                               (:file "tm-quantified")
                               (:file "tm-print")

                               ;; no destructive functions, but allows copies
                               (:file "nd-tm-def")
#|
                               (:file "nd-tm-primitives")
                               (:file "nd-tm-derived") 
                               (:file "nd-tm-quantified")
                               (:file "nd-tm-subspace")
                               (:file "length")
                               (:file "location")

                               ;; includes destructive functions, but not copies
                               (:file "solo-tm-def")
                               (:file "solo-tm-primitives")
                               (:file "solo-tm-derived")

                               ;; a solo-tm implemenation
                               (:file "list-solo-tm-def")
                               (:file "list-solo-tm-primitives")

                               (:file "tm-def")
                               (:file "tm-state")
                               (:file "entanglement")
                               (:file "print-machine")
                               (:file "tm-primitives")
                               (:file "entanglement")
                               (:file "entanglement-scope")

                               (:file "tm-derived-1")
                               (:file "entanglement-1")
                               (:file "tm-quantifiers-1")
                               (:file "tm-derived-2")
                               (:file "length")

                               (:file "tm-subspace")
                               (:file "location")

                               (:file "convert")


                             ;; projective void
                               (:file "tm-void")
                               |#
                               ))


               (:module "src-list"
                 :components (
                             ;; interface definition

#|
                             ;; list implementation
                               (:file "tm-list-mk")
                               (:file "tm-list-primitives")
                               (:file "tm-list-derived")
                               (:file "tm-list-length")
                               (:file "tm-list-convert")

                             ;; dataflow
                               (:file "dataflow")

|#                             
                               ))
#|

               (:module "src-generators"
                 :components (
                               (:file "tm-DE")
                               ))

               (:module "src-transforms"
                 :components (
                             ;; transforms
                               (:file "tm-region")
                               (:file "buffers")

                               (:file "tm-depth-mk")
                               (:file "tm-depth-primitives")
                               (:file "tm-depth-convert")


                               (:file "tm-breadth-mk")
                               (:file "tm-breadth-primitives")
                               (:file "tm-breadth-convert")
                            ))

               (:module "test-list"
                :components (
                              (:file "tm-state")
                              
                              (:file "tm-derived")
                              (:file "tm-subspace")
                              (:file "tm-quantifiers")

                              (:file "length")
                              (:file "location")

                              (:file "tm-list-mk")
                              (:file "tm-list-primitives")
                              (:file "tm-list-derived")
                              ))


               (:module "test-generators"
                :components (
                             (:file "tm-DE")

                              ))

               (:module "test-transforms"
                :components (
                              (:file "tm-region")
                              (:file "buffers")
;;                              (:file "tm-depth")
;;                              (:file "tm-breadth")
                              ))
|#

#|


               (:module "src-array"
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
  
  



