;;; ecloud-utils-test.el --- Tests for ecloud utils.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'dash)
(require 'magit)
(require 'ecloud-utils)
(require 'buttercup)


(describe "Reading Integer from minibuffer"

          (it "Raises a user-error on empty input"
              (spy-on 'read-from-minibuffer :and-return-value "")
              (expect (ecloud-read-int "enter an int") :to-throw 'user-error))

          (it "Raises a user-error on invalid input"
              (spy-on 'read-from-minibuffer :and-return-value "hello")
              (expect (ecloud-read-int "enter an int") :to-throw 'user-error))

          (it "Returns integer on integer input"
              (spy-on 'read-from-minibuffer :and-return-value "12345")
              (expect (ecloud-read-int "enter an int") :to-equal 12345))

          (it "Trims leading and trailing space on integer input"
              (spy-on 'read-from-minibuffer :and-return-value "  12345  ")
              (expect (ecloud-read-int "enter an int") :to-equal 12345))
          )
