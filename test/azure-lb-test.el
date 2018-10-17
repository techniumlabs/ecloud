;;; azure-lb-test.el --- Tests for azure lb.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-lb)


(describe "Azure Lb"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure lb)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-lb-list-response.json")
                                    'azure-lb)))

                    (it "Should return correct name and id"
                        (expect (has-resource azure-lb (("name" "sfabric-lb")
                                                        ("id" "/subscriptions/deadbeef-73a9-44e3-b70f-beed06029c60/resourceGroups/technium/providers/Microsoft.Network/loadBalancers/sfabric-lb")))
                                :not :to-be nil)
                        (expect (ecloud-resource-count azure-lb) :to-be 1))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(lb))
                            (expect (s-trim  "azure lb
name           location          resourceGroup     
sfabric-lb     australiaeast     technium") :to-match
                                    (s-trim (substring-no-properties (buffer-string)))))))
                    )

          (describe "When Parsing a null response"

                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure lb)
                                   (ecloud-parse-resource-data
                                    ""
                                    'azure-lb)))
                    (it "Should not have any entries"
                        (expect (ecloud-resource-count azure-lb) :to-be 0))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(lb))
                            (expect (s-trim  "azure lb
No lb found") :to-match
(s-trim (substring-no-properties (buffer-string))))))))
          )

