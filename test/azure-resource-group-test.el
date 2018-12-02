;;; azure-resource-group-test.el --- Tests for azure Resourece Group.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-resource-group)

(describe "Azure Resource Group"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure resource-group)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-resource-group-list-response.json")
                                    'azure-resource-group)))
                    (it "Should return correct name and id"
                        (expect (has-resource azure-resource-group (("name" "aci-example")
                                                                    ("id" "/subscriptions/deadbeef-5c15-4b08-b31e-0abaf2f2b924/resourceGroups/aci-example")))
                                :not :to-be nil)
                        (expect (ecloud-resource-count azure-resource-group) :to-be 6))))
