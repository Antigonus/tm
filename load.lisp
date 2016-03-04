#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
|#

(require "asdf")
(require "closer-mop")
(pushnew (truename ".") asdf:*central-registry* :test #'equal)
(asdf:load-system "le")
(require "le")
(use-package 'le)



