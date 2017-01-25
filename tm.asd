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
                               (:file "tm-type")
                               (:file "tm-mk")
                               (:file "tm-decl-only")
                               (:file "tm-generic")
                               (:file "tm-quantifiers")
                               (:file "tm-quantified")
;;                               (:file "tm-print")

                               (:file "list-tm-type")
                               (:file "list-tm-mk")
                               (:file "list-tm-definitions")
                               (:file "list-tm-specialized-generic")

                               ;; no destructive functions, but has entangled copy functions
                               (:file "nd-tm-type")
                               (:file "nd-tm-decl-only")
                               (:file "nd-entanglement")
                               (:file "nd-tm-generic") 

                               (:file "nd-tm-quantified")

                               (:file "list-nd-tm-type")
                               (:file "list-nd-tm-mk")
                               (:file "list-nd-tm-definitions")

                               ;; includes destructive functions, but no entangled copy functions
                               (:file "solo-tm-type")
                               (:file "solo-tm-decl-only")
                               (:file "solo-tm-quantified")

                               ;; a solo-tm implemenation
                               (:file "list-solo-tm-type")
                               (:file "list-solo-tm-definitions")

                               ;; includes destructive functions, and entangled copy functions
                               (:file "ea-tm-type")
                               (:file "ea-entanglement")
                               (:file "ea-tm-generic")

                               (:file "list-ea-tm-type")
                               (:file "list-ea-tm-mk")

;;  (:file "ts-tm-def")
;;                               (:file "nd-tm-subspace") ; issues with 'manifold'
;;                               (:file "tm-subspace")
;;                               (:file "convert")

                               ))

               (:module "test-list"
                :components (
                              ;; generic interface tests
                              (:file "list-tm-definitions")
                              (:file "tm-generic")
                              (:file "tm-quantifiers")
                              (:file "tm-quantified")

                              (:file "list-nd-tm-definitions")
                              (:file "nd-tm-generic")
                              (:file "nd-tm-quantifiers")
                              (:file "nd-tm-quantified")

                              (:file "list-solo-tm-definitions")
                              (:file "solo-tm-quantified")
                              (:file "ea-tm-generic")
                              ))

               (:module "src-tr"
                 :components (
                               (:file "identity")
                               ))

               (:module "test-tr"
                :components (
                              (:file "identity")
                              ))

#|

               (:module "src-generators" 
                 :components (
                               (:file "length")
                               (:file "location")
                               (:file "tm-DE")
                               ))




               (:module "test-generators"
                :components (
                              (:file "length")
                              (:file "location")

                             (:file "tm-DE")

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
  
  



