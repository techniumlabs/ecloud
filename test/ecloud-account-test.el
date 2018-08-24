;;; ecloud-account-test.el --- Tests for azure account.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(declare-function test-helper-json-resource "test-helper.el")
(defconst sample-get-account-list-response (test-helper-json-resource "azure-account-list-response.json"))

(ert-deftest ecloud-account-test--ecloud-fetch-resources ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure account)
   (ecloud-parse-resource-data sample-get-account-list-response 'azure-account)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "account"))
   (should (has-resource azure-account (("name" "Microsoft Azure Sponsorship")
                                        ("id" "02ea3122-ecd3-4a0d-97ee-deadbeefed01"))))))

(ert-deftest ecloud-account-test--ecloud-fetch-resources-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure account)
   (ecloud-parse-resource-data nil 'azure-account)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "account"))
   (should (equal (ecloud-resource-count azure-account) 0))
   ))

(defconst azure-account-list-view-result
  (s-trim-left "
azure account
name                            state       isDefault     
Technium Labs Australia         Enabled     true          
Microsoft Azure Sponsorship     Enabled     false"))

(ert-deftest ecloud-account-test--ecloud-insert-list-views ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure account)
   (ecloud-parse-resource-data sample-get-account-list-response 'azure-account)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(account))
       (should (equal azure-account-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(defconst azure-account-list-view-empty-result
  (s-trim-left "
azure account
No account found"))

(ert-deftest ecloud-account-test--ecloud-insert-list-views-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure account)
   (ecloud-parse-resource-data nil 'azure-account)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(account))
       (should (equal azure-account-list-view-empty-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(provide 'ecloud-account-test)

;;; ecloud-account-test.el ends here
