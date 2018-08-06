;;; ecloud-account-test.el --- Tests for azure account.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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
   ))


(provide 'ecloud-account-test)

;;; ecloud-account-test.el ends here
