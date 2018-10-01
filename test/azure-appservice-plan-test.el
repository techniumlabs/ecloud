;;; ecloud-appservice-plan-test.el --- Tests for azure appservice-plan.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(declare-function test-helper-json-resource "test-helper.el")
(defconst sample-get-appservice-plan-list-response (test-helper-json-resource "azure-appservice-plan-list-response.json"))

(ert-deftest ecloud-appservice-plan-test--ecloud-fetch-resources ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure appservice-plan)
   (ecloud-parse-resource-data sample-get-appservice-plan-list-response 'azure-appservice-plan)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "appservice-plan"))
   (should (has-resource azure-appservice-plan (("name" "AustraliaEastPlan")
                                                ("id" "/subscriptions/deadbeef-73a9-44e3-b70f-ee9206029c60/resourceGroups/techniumlabs/providers/Microsoft.Web/serverfarms/AustraliaEastPlan"))))))

(ert-deftest ecloud-appservice-plan-test--ecloud-fetch-resources-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure appservice-plan)
   (ecloud-parse-resource-data nil 'azure-appservice-plan)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "appservice-plan"))
   (should (equal (ecloud-resource-count azure-appservice-plan) 0))
   ))

(defconst azure-appservice-plan-list-view-result
  (s-trim-left "
azure appservice-plan
name                  kind            resourceGroup     
AustraliaEastPlan     functionapp     techniumlabs"))

(ert-deftest ecloud-appservice-plan-test--ecloud-insert-list-views ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure appservice-plan)
   (ecloud-parse-resource-data sample-get-appservice-plan-list-response 'azure-appservice-plan)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(appservice-plan))
       (should (equal azure-appservice-plan-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(defconst azure-appservice-plan-list-view-empty-result
  (s-trim-left "
azure appservice-plan
No appservice-plan found"))

(ert-deftest ecloud-appservice-plan-test--ecloud-insert-list-views-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure appservice-plan)
   (ecloud-parse-resource-data nil 'azure-appservice-plan)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(appservice-plan))
       (should (equal azure-appservice-plan-list-view-empty-result
                      (s-trim (substring-no-properties (buffer-string)))))))))


(provide 'ecloud-appservice-plan-test)

;;; ecloud-appservice-plan-test.el ends here
