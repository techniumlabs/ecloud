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

(describe "Calling Run json command"
          (before-each (spy-on 'ecloud-run-command))
          (it "Should call run command"
              (ecloud-run-json-command '("az" "account" "list") nil nil)
              (expect 'ecloud-run-command :to-have-been-called)))

(describe "Calling run command"
          (before-each (spy-on 'make-process)
                       (spy-on 'generate-new-buffer :and-call-through))
          (it "Should create process"
              (ecloud-run-command '("az" "account" "list") '("") (lambda (buf) (ecloud-parse-json-buffer buf)))
              (expect 'generate-new-buffer :to-have-been-called)
              (expect 'make-process :to-have-been-called)))
