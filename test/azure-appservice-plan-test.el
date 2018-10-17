;;; azure-appservice-plan-test.el --- Tests for azure appservice-plan.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-appservice-plan)


(describe "Azure Appservice-Plan"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure appservice-plan)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-appservice-plan-list-response.json")
                                    'azure-appservice-plan)))

                    (it "Should return correct name and id"
                        (expect (has-resource azure-appservice-plan (("name" "AustraliaEastPlan")
                                                         ("id" "/subscriptions/deadbeef-73a9-44e3-b70f-ee9206029c60/resourceGroups/techniumlabs/providers/Microsoft.Web/serverfarms/AustraliaEastPlan")))
                                :not :to-be nil)
                        (expect (ecloud-resource-count azure-appservice-plan) :to-be 1))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(appservice-plan))
                            (expect (s-trim  "azure appservice-plan
name                  kind            resourceGroup     
AustraliaEastPlan     functionapp     techniumlabs") :to-match
                                    (s-trim (substring-no-properties (buffer-string)))))))
                    )

          (describe "When Parsing a null response"

                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure appservice-plan)
                                   (ecloud-parse-resource-data
                                    ""
                                    'azure-appservice-plan)))
                    (it "Should not have any entries"
                        (expect (ecloud-resource-count azure-appservice-plan) :to-be 0))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(appservice-plan))
                            (expect (s-trim  "azure appservice-plan
No appservice-plan found") :to-match
(s-trim (substring-no-properties (buffer-string))))))))
          )

