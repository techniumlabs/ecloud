;;; ecloud-routetable-test.el --- Tests for azure routetable.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(declare-function test-helper-json-resource "test-helper.el")
(defconst sample-get-routetable-list-response (test-helper-json-resource "azure-routetable-list-response.json"))

(ert-deftest ecloud-routetable-test--ecloud-fetch-resources ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure routetable)
   (ecloud-parse-resource-data sample-get-routetable-list-response 'azure-routetable)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "routetable"))
   (should (has-resource azure-routetable (("name" "aks-agentpool-33686480-routetable")
                                           ("id" "/subscriptions/deadbeef-beef-dead-b70f-ee9206029c60/resourceGroups/gnuherd/providers/Microsoft.Network/routeTables/aks-agentpool-33686480-routetable"))))))

(ert-deftest ecloud-routetable-test--ecloud-fetch-resources-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure routetable)
   (ecloud-parse-resource-data nil 'azure-routetable)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "routetable"))
   (should (equal (ecloud-resource-count azure-routetable) 0))
   ))

(defconst azure-routetable-list-view-result
  (s-trim-left "
azure routetable
name                                  location          
aks-agentpool-33686480-routetable     australiaeast"))

(ert-deftest ecloud-routetable-test--ecloud-insert-list-views ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure routetable)
   (ecloud-parse-resource-data sample-get-routetable-list-response 'azure-routetable)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(routetable))
       (should (equal azure-routetable-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(defconst azure-routetable-list-view-empty-result
  (s-trim-left "
azure routetable
No routetable found"))

(ert-deftest ecloud-routetable-test--ecloud-insert-list-views-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure routetable)
   (ecloud-parse-resource-data nil 'azure-routetable)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(routetable))
       (should (equal azure-routetable-list-view-empty-result
                      (s-trim (substring-no-properties (buffer-string)))))))))


(provide 'ecloud-routetable-test)

;;; ecloud-routetable-test.el ends here
