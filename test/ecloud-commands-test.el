;;; ecloud-commands-test.el --- Tests for ecloud commands.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'dash)
(require 'magit)
(require 'ecloud-state)
(require 'ecloud-model)
(require 'ecloud-commands)

(describe "json buffer"
          
          (it "should be parsed correctly"
              (with-temp-buffer
                (insert (test-helper-string-resource "azure-vnet-list-response.json"))
                (expect (ecloud-parse-json-buffer (current-buffer)) :to-equal (test-helper-json-resource "azure-vnet-list-response.json"))))

          (it "Should return nil if on empty input"
              (with-temp-buffer
                (insert "")
                (expect (ecloud-parse-json-buffer (current-buffer)) :to-equal nil))))
