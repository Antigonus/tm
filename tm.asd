#|
Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
Released under the MIT License (MIT)
See LICENSE.txt

|#


(in-package :asdf-user)

(defsystem #:tm
  :name "tm"
  :version "0.7"
  :author "Thomas W. Lynch <thomas.lynch@reasoningtechnology.com>"
  :description "Formalized Iteration Library for Common LISP"
  :license "MIT License"
  :depends-on ("local-time" "bordeaux-threads")
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
                               (:file "do")
                               ))

               (:module "src-test"
                 :components (
                               (:file "print")
                               (:file "test")
                               ))

               (:module "src-cell"
                 :components (
                               (:file "interface")
                               (:file "list")
                               (:file "bilist")
                               ))

               (:module "src-tape"
                 :components (
                               (:file "interface")
                               (:file "list")
;;                               (:file "bilist")
                               ))

               (:module "src-tape-machine"
                 :components (
;;                               (:file "interface")
;;                               (:file "tm")
                               ))


#|
               (:module "src-list" ; both the list and generic interface
                 :components (
                               (:file "tm-type")
                               (:file "tm-decls")
                               (:file "tm-generic")
                               (:file "quantifiers")
                               (:file "tm-quantified")

                               (:file "list-tm-type")
                               (:file "list-tm-definitions")
                               (:file "list-tm-specialized-generic")

                               ;; no destructive functions, but has entangled copy functions
                               (:file "nd-tm-type")
                               (:file "nd-tm-decls")
                               (:file "nd-tm-generic") 

                               (:file "nd-tm-quantified")

                               (:file "list-nd-tm-type")
                               (:file "list-nd-tm-definitions")

                               ;; includes destructive functions, but no entangled copy functions
                               (:file "solo-tm-type")
                               (:file "solo-tm-decls")
                               (:file "solo-tm-quantified")

                               ;; a solo-tm implemenation
                               (:file "list-solo-tm-type")
                               (:file "list-solo-tm-definitions")

                               ;; this machine must be managed
                               (:file "haz-tm-type")
                               (:file "haz-tm-decls")
                               (:file "list-haz-tm-type")

                               ;; bi-directional list, support -s
                               (:file "bi-tm-decls")
                               (:file "bilist")
                               (:file "bilist-tm-type")
                               (:file "bilist-tm-definitions")
                               (:file "bilist-nd-tm-type")
                               (:file "bilist-nd-tm-definitions")
                               (:file "bilist-solo-tm-type")
                               (:file "bilist-solo-tm-definitions")
                               (:file "bilist-haz-tm-type")
                               (:file "bilist-haz-definitions")

;;                               (:file "nd-tm-subspace") ; issues with 'manifold'
;;                               (:file "tm-subspace")
;;                               (:file "convert")

                               (:file "copy")
                               ))

               (:module "src-generators"
                 :components (
                               (:file "recursive")
                               ))


               (:module "src-tr"
                 :components (
                               (:file "identity")
                               (:file "ensemble")
                               ))

               (:module "src-second-level"
                 :components (
                               (:file "status-type")

                               (:file "status-tm")
                               (:file "status-quantifiers")
                               (:file "status-quantified")
                               (:file "status-copy")
                               (:file "status=")

                               (:file "status-abandoned")
                               (:file "status-empty")
                               (:file "status-parked")
                               (:file "status-active")

                               (:file "ea-type")
                               (:file "ea-tm")

                               (:file "ea-abandoned")
                               (:file "ea-empty")
                               (:file "ea-parked")
                               (:file "ea-active")
                               (:file "ea-parked-active")

                               (:file "ts1-type")
                               (:file "ts1-tm")
                               ))


                (:module "src-array"
                  :components (
                                (:file "tiled-number")
                                ))
|#
              (:module "test-0"
                 :components (
                               (:file "list-qL")
                               (:file "functions")
                               ))

               (:module "test-cell"
                 :components (
                               (:file "list")
                               (:file "bilist")
                               ))

               (:module "test-tape"
                 :components (
                               (:file "list")
                               ))


#|
               (:module "test-list"
                :components (
                              ;; generic interface tests
                              (:file "bilist")
                              (:file "list-nd-tm-definitions")
                              (:file "list-solo-tm-definitions")
                              (:file "list-tm-definitions")
                              (:file "nd-tm-generic")
                              (:file "nd-tm-quantified")
                              (:file "nd-tm-quantifiers")
                              (:file "solo-tm-quantified")
                              (:file "tm-generic")
                              (:file "tm-quantified")
                              (:file "quantifiers")
                              (:file "copy")
                              ))

               (:module "test-generators"
                 :components (
                               (:file "recursive")
                               ))

               (:module "test-tr"
                :components (
                              (:file "identity")
                              (:file "ensemble")
                              ))

               (:module "test-second-level"
                 :components (
                               (:file "status")
                               (:file "status-quantifiers")
                               (:file "status-quantified")
;;                               (:file "status=")
                               (:file "ea")
                               (:file "ts1")
                               (:file "copy")
                               ))

               (:module "test-array"
                 :components (
                               (:file "tiled-number")
                               ))
|#
               ))
  
  



