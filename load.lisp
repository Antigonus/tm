#|
  Copyright (c) 2016 Thomas W. Lynch and Reasoning Technology Inc.
  Released under the MIT License (MIT)
  See LICENSE.txt

|#

(require "asdf")
(pushnew (truename "/home/tm") asdf:*central-registry* :test #'equal)
(asdf:load-system "tm")
(require "tm")
(use-package 'tm)


