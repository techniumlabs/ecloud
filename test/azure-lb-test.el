;;; ecloud-lb-test.el --- Tests for azure lb.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(declare-function test-helper-json-resource "test-helper.el")
(defconst sample-get-lb-list-response (test-helper-json-resource "azure-lb-list-response.json"))

(ert-deftest ecloud-lb-test--ecloud-fetch-resources ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure lb)
   (ecloud-parse-resource-data sample-get-lb-list-response 'azure-lb)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "lb"))
   (should (has-resource azure-lb (("name" "sfabric-lb")
                                   ("id" "/subscriptions/deadbeef-73a9-44e3-b70f-beed06029c60/resourceGroups/technium/providers/Microsoft.Network/loadBalancers/sfabric-lb"))))))

(ert-deftest ecloud-lb-test--ecloud-fetch-resources-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure lb)
   (ecloud-parse-resource-data nil 'azure-lb)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "lb"))
   (should (equal (ecloud-resource-count azure-lb) 0))
   ))

(defconst azure-lb-list-view-result
  (s-trim-left "
azure lb
name           location          resourceGroup     
sfabric-lb     australiaeast     technium"))

(ert-deftest ecloud-lb-test--ecloud-insert-list-views ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure lb)
   (ecloud-parse-resource-data sample-get-lb-list-response 'azure-lb)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(lb))
       (should (equal azure-lb-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(defconst azure-lb-list-view-empty-result
  (s-trim-left "
azure lb
No lb found"))

(ert-deftest ecloud-lb-test--ecloud-insert-list-views-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure lb)
   (ecloud-parse-resource-data nil 'azure-lb)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(lb))
       (should (equal azure-lb-list-view-empty-result
                      (s-trim (substring-no-properties (buffer-string)))))))))


(provide 'ecloud-lb-test)

;;; ecloud-lb-test.el ends here
