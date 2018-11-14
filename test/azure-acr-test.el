;;; azure-acr-test.el --- Tests for azure acr.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-acr)
(require 'eieio-opt)
(eval-when-compile (require 'cl))


(describe "Azure Acr"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-acr-list-response.json")
                                    'azure-acr)))

                    (it "Should return correct name and id"
                        (expect (has-resource azure-acr (("name" "techniumlabs")
                                                         ("id" "/subscriptions/12345678-73a9-44e3-b70f-ee9206029c60/resourceGroups/azure-operator/providers/Microsoft.ContainerRegistry/registries/techniumlabs")))
                                :not :to-be nil)
                        (expect (ecloud-resource-count azure-acr) :to-be 1)
                        (-let ((obj (first (ecloud-state--get-all-resource-type "azure" "acr"))))
                          (expect (ecloud-resource-name  obj) :to-match "techniumlabs"))
                        )

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(acr))
                            (expect (s-trim  "azure acr
name             loginServer                 location       
techniumlabs     techniumlabs.azurecr.io     westeurope") :to-match
                                    (s-trim (substring-no-properties (buffer-string)))))))
                    )

          (describe "When Parsing a null response"

                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure acr)
                                   (ecloud-parse-resource-data
                                    ""
                                    'azure-acr)))
                    (it "Should not have any entries"
                        (expect (ecloud-resource-count azure-acr) :to-be 0))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(acr))
                            (expect (s-trim  "azure acr
No acr found") :to-match
(s-trim (substring-no-properties (buffer-string))))))))
          )

