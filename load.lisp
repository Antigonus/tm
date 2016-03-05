#|
  Copyright (C) 2014 Reasoning Technology  All Rights Reserved.
  COMPANY CONFIDENTIAL Reaosning Technology
  author: thomas w. lynch
  
|#

(require "asdf")
(pushnew (truename ".") asdf:*central-registry* :test #'equal)
(asdf:load-system "tm")
(require "tm")
(use-package 'tm)



