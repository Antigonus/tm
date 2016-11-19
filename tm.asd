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

               (:module "src-list" ; both the list and generic interface
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

                               ;; no destructive functions, but has entangled copy functions
                               (:file "nd-tm-def")
                               (:file "nd-tm-primitives")
                               (:file "nd-tm-derived") 
                               (:file "nd-tm-quantifiers")
                               (:file "nd-tm-quantified")

                               (:file "list-nd-tm-def")
                               (:file "list-nd-tm-mk")
                               (:file "list-nd-tm-primitives")

                               ;; includes destructive functions, but no entangled copy functions
                               (:file "solo-tm-def")
                               (:file "solo-tm-primitives")
                               (:file "solo-tm-quantified")

                               ;; a solo-tm implemenation
                               (:file "list-solo-tm-def")
                               (:file "list-solo-tm-primitives")

                               ;; includes destructive functions, and entangled copy functions
                               (:file "ea-tm-def")
                               (:file "ea-entanglement")
                               (:file "ea-tm-primitives")
                               (:file "ea-tm-derived")

                               (:file "list-ea-tm-def")
                               (:file "list-ea-tm-mk")

;;  (:file "ts-tm-def")
;;                               (:file "nd-tm-subspace") ; issues with 'manifold'
;;                               (:file "tm-subspace")
;;                               (:file "convert")

                               ))

               (:module "test-list"
                :components (
                              ;; generic interface tests
                              (:file "list-tm-primitives")
                              (:file "tm-derived")
                              (:file "tm-quantifiers")
                              (:file "tm-quantified")

                              (:file "list-nd-tm-primitives")
                              (:file "nd-tm-derived")
                              (:file "nd-tm-quantifiers")
                              (:file "nd-tm-quantified")

                              (:file "list-solo-tm-primitives")
                              (:file "solo-tm-quantified")

                              (:file "ea-tm-derived")
                              ))

#|

               (:module "src-generators" 
                 :components (
                               (:file "length")
                               (:file "location")
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
                              (:file "length")
                              (:file "location")

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
  
  



