;;; ecloud-nsg-test.el --- Tests for azure nsg.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(declare-function test-helper-json-resource "test-helper.el")
(defconst sample-get-nsg-list-response (test-helper-json-resource "azure-nsg-list-response.json"))

(ert-deftest ecloud-nsg-test--ecloud-fetch-resources ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure nsg)
   (ecloud-parse-resource-data sample-get-nsg-list-response 'azure-nsg)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "nsg"))
   (should (has-resource azure-nsg (("name" "aks-agentpool-33686480-nsg")
                                    ("id" "/subscriptions/deadbeef-73a9-44e3-b70f-ee9206029c60/resourceGroups/cattle/providers/Microsoft.Network/networkSecurityGroups/aks-agentpool-33686480-nsg"))))))

(ert-deftest ecloud-nsg-test--ecloud-fetch-resources-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure nsg)
   (ecloud-parse-resource-data nil 'azure-nsg)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "nsg"))
   (should (equal (ecloud-resource-count azure-nsg) 0))
   ))

(defconst azure-nsg-list-view-result
  (s-trim-left "
azure nsg
name                           location          
aks-agentpool-33686480-nsg     australiaeast"))

(ert-deftest ecloud-nsg-test--ecloud-insert-list-views ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure nsg)
   (ecloud-parse-resource-data sample-get-nsg-list-response 'azure-nsg)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(nsg))
       (should (equal azure-nsg-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(defconst azure-nsg-list-view-empty-result
  (s-trim-left "
azure nsg
No nsg found"))

(ert-deftest ecloud-nsg-test--ecloud-insert-list-views-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure nsg)
   (ecloud-parse-resource-data nil 'azure-nsg)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(nsg))
       (should (equal azure-nsg-list-view-empty-result
                      (s-trim (substring-no-properties (buffer-string)))))))))


(provide 'ecloud-nsg-test)

;;; ecloud-nsg-test.el ends here
