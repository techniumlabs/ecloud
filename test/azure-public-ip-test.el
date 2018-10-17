;;; azure-public-ip-test.el --- Tests for azure public ip.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-public-ip)


(describe "Azure public ip address"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure public-ip)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-public-ip-list-response.json")
                                    'azure-public-ip)))

                    (it "Should return correct name and id"
                        (expect (has-resource azure-public-ip (("name" "technium-ip")
                                                               ("id" "/subscriptions/12345678-73a9-44e3-b70f-ee9206029c60/resourcegroups/technium/providers/microsoft.network/publicipaddresses/technium-ip")))
                                :not :to-be nil)
                        (expect (ecloud-resource-count azure-public-ip) :to-be 1))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(public-ip))
                            (expect (s-trim  "azure public-ip
name            location          
technium-ip     australiaeast") :to-match
                                    (s-trim (substring-no-properties (buffer-string)))))))
                    )

          (describe "When Parsing a null response"

                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure public-ip)
                                   (ecloud-parse-resource-data
                                    ""
                                    'azure-public-ip)))
                    (it "Should not have any entries"
                        (expect (ecloud-resource-count azure-public-ip) :to-be 0))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(public-ip))
                            (expect (s-trim  "azure public-ip
No public-ip found") :to-match
(s-trim (substring-no-properties (buffer-string))))))))
          )

