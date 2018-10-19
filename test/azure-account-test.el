;;; azure-account-test.el --- Tests for azure account.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-account)


(describe "Azure Account"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure account)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "azure-account-list-response.json")
                                    'azure-account)))

                    (it "Should return correct name and id"
                        (expect (has-resource azure-account (("name" "Microsoft Azure Sponsorship")
                                                             ("id" "02ea3122-ecd3-4a0d-97ee-deadbeefed01")))
                                :not :to-be nil)
                        (expect (ecloud-resource-count azure-account) :to-be 2))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(account))
                            (expect (s-trim  "azure account
name                            state       isDefault     
Technium Labs Australia         Enabled     true          

Cloud Name: AzureCloud
Tenant Id: deadbeef-2faa-4942-aa9e-deadbeef6a63
Username: technium.ecloud@example.com
User Type: user

Microsoft Azure Sponsorship     Enabled     false         

Cloud Name: AzureCloud
Tenant Id: aaaabbc7-dead-beef-aa9e-e26ab8bc6a63
Username: ecloud-user@example.com
User Type: user") :to-match
                                    (s-trim (substring-no-properties (buffer-string)))))))
                    )

          (describe "When Parsing a null response"

                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model azure account)
                                   (ecloud-parse-resource-data
                                    ""
                                    'azure-account)))
                    (it "Should not have any entries"
                        (expect (ecloud-resource-count azure-account) :to-be 0))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'azure '(account))
                            (expect (s-trim  "azure account
No account found") :to-match
(s-trim (substring-no-properties (buffer-string))))))))
          )

