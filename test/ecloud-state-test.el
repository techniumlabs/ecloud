;;; ecloud-state-test.el --- Tests for ecloud state.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'dash)
(require 'magit)
(require 'ecloud-state)
(require 'buttercup)

(describe "State on initialization"
          (it "should be empty"
              (ecloud-state-init)
              (expect (ht-size (ecloud-state)) :to-equal 0))

          (it "Should have a cache directory created"
              (ecloud-state-init)
              (expect (f-dir? (format "%s/ecloud" pcache-directory)))))

