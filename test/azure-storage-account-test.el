;;; ecloud-storage-account-test.el --- Tests for azure storage-account.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(declare-function test-helper-json-resource "test-helper.el")
(defconst sample-get-storage-account-list-response (test-helper-json-resource "azure-storage-account-list-response.json"))

(ert-deftest ecloud-storage-account-test--ecloud-fetch-resources ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure storage-account)
   (ecloud-parse-resource-data sample-get-storage-account-list-response 'azure-storage-account)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "storage-account"))
   (should (has-resource azure-storage-account (("name" "techniumlab")
                                                ("id" "/subscriptions/deadbeef-73a9-44e3-b70f-ee9206029c60/resourceGroups/techniumlab/providers/Microsoft.Storage/storageAccounts/techniumlab"))))))

(ert-deftest ecloud-storage-account-test--ecloud-fetch-resources-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure storage-account)
   (ecloud-parse-resource-data nil 'azure-storage-account)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "storage-account"))
   (should (equal (ecloud-resource-count azure-storage-account) 0))
   ))

(defconst azure-storage-account-list-view-result
  (s-trim-left "
azure storage-account
name                         kind            location          
techniumweb                  StorageV2       australiaeast     
function5csq4xefxww7i        Storage         australiaeast     
techniumlab                  BlobStorage     australiaeast     
cs2deadbeef73a9x44e3xb70     Storage         eastus"))

(ert-deftest ecloud-storage-account-test--ecloud-insert-list-views ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure storage-account)
   (ecloud-parse-resource-data sample-get-storage-account-list-response 'azure-storage-account)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(storage-account))
       (should (equal azure-storage-account-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(defconst azure-storage-account-list-view-empty-result
  (s-trim-left "
azure storage-account
No storage-account found"))

(ert-deftest ecloud-storage-account-test--ecloud-insert-list-views-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure storage-account)
   (ecloud-parse-resource-data nil 'azure-storage-account)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(storage-account))
       (should (equal azure-storage-account-list-view-empty-result
                      (s-trim (substring-no-properties (buffer-string)))))))))


(provide 'ecloud-storage-account-test)

;;; ecloud-storage-account-test.el ends here
