;;; ecloud-vnet-test.el --- Tests for azure vnet.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(declare-function test-helper-json-resource "test-helper.el")
(defconst sample-get-vnet-list-response (test-helper-json-resource "azure-vnet-list-response.json"))

(ert-deftest ecloud-vnet-test--ecloud-fetch-resources ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure vnet)
   (ecloud-parse-resource-data sample-get-vnet-list-response 'azure-vnet)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "vnet"))
   (should (has-resource azure-vnet (("name" "aks-vnet-deadbeef")
                                     ("id" "/subscriptions/deadbeef-73a9-44e3-b70f-deadbeef9c60/resourceGroups/gnuherd/providers/Microsoft.Network/virtualNetworks/aks-vnet-deadbeef"))))))

(ert-deftest ecloud-vnet-test--ecloud-fetch-resources-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure vnet)
   (ecloud-parse-resource-data nil 'azure-vnet)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "vnet"))
   (should (equal (ecloud-resource-count azure-vnet) 0))
   ))

(defconst azure-vnet-list-view-result
  (s-trim-left "
azure vnet
name                  location          
aks-vnet-deadbeef     australiaeast"))

(ert-deftest ecloud-vnet-test--ecloud-insert-list-views ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure vnet)
   (ecloud-parse-resource-data sample-get-vnet-list-response 'azure-vnet)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(vnet))
       (should (equal azure-vnet-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(defconst azure-vnet-list-view-empty-result
  (s-trim-left "
azure vnet
No vnet found"))

(ert-deftest ecloud-vnet-test--ecloud-insert-list-views-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure vnet)
   (ecloud-parse-resource-data nil 'azure-vnet)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(vnet))
       (should (equal azure-vnet-list-view-empty-result
                      (s-trim (substring-no-properties (buffer-string)))))))))


(provide 'ecloud-vnet-test)

;;; ecloud-vnet-test.el ends here
