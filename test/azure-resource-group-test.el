;;; azure-group-test.el --- Tests for azure Resourece Group.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-group)

(describe "Azure Resource Group"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure group)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-group-list-response.json")
                                    'azure-group)))
                    (it "Should return correct name and id"
                        (expect (has-resource azure-group (("name" "aci-example")
                                                                    ("id" "/subscriptions/deadbeef-5c15-4b08-b31e-0abaf2f2b924/resourceGroups/aci-example")))
                                :not :to-be nil)
                        (expect (ecloud-resource-count azure-group) :to-be 6))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(group))
                            (expect (s-trim  "azure group
name                                  location          state         
myapp                                 australiaeast     Succeeded     
hiberapp                              australiaeast     Succeeded     
techniumlabs                          australiaeast     Succeeded     
cloud-shell-storage-southeastasia     southeastasia     Succeeded     
cloud-shell-storage-eastus            eastus            Succeeded     
aci-example                           eastus            Succeeded") :to-match
(s-trim (substring-no-properties (buffer-string))))))))

          (describe "When Parsing a null response"

                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure group)
                                   (ecloud-parse-resource-data
                                    ""
                                    'azure-group)))
                    (it "Should not have any entries"
                        (expect (ecloud-resource-count azure-group) :to-be 0))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(group))
                            (expect (s-trim  "azure group
No group found") :to-match (s-trim (substring-no-properties (buffer-string))))))))
          )
