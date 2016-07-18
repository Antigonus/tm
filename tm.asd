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

               (:module "src-list"
                 :components (
                               (:file "tm-def")
                               (:file "tm-mk")
                               (:file "tm-primitives")
                               (:file "tm-derived")
                               (:file "tm-quantifiers")
                               (:file "tm-quantified")
                               (:file "tm-print")

                               (:file "list-tm-def")
                               (:file "list-tm-mk")
                               (:file "list-tm-primitives")
                               (:file "list-tm-derived")

                               ;; no destructive functions, but allows entangle copy functions
                               (:file "nd-tm-def")
                               (:file "nd-tm-primitives")
                               (:file "nd-tm-derived") 
                               (:file "nd-tm-quantifiers")
                               (:file "nd-tm-quantified")
                               (:file "length")
                               (:file "location")

                               (:file "list-nd-tm-def")
                               (:file "list-nd-tm-primitives")

                               ;; includes destructive functions, but no entangle copy functions
                               (:file "solo-tm-def")
                               (:file "solo-tm-primitives")
                               (:file "solo-tm-quantified")

                               ;; a solo-tm implemenation
                               (:file "list-solo-tm-def")
                               (:file "list-solo-tm-primitives")


                               ;; includes destructive functions, and entangle copy functions
                               (:file "entanglement")
                               (:file "multi-tm-def")
                               (:file "multi-tm-mk")
                               (:file "multi-tm-primitives")
                               (:file "multi-tm-derived")

                               (:file "list-multi-tm-def")


;;  (:file "threaded-tm-def")
;;                               (:file "nd-tm-subspace") ; issues with 'manifold'
;;                               (:file "tm-subspace")
;;                               (:file "convert")

                               ))

               (:module "test-list"
                :components (
                              (:file "list-tm")
#|
                              (:file "tm-state")
                              
                              (:file "tm-derived")
                              (:file "tm-subspace")
                              (:file "tm-quantifiers")

                              (:file "length")
                              (:file "location")

                              (:file "tm-list-mk")
                              (:file "tm-list-primitives")
                              (:file "tm-list-derived")
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
  
  



