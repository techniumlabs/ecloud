;;; ecloud-group-test.el --- Tests for azure group.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(declare-function test-helper-json-resource "test-helper.el")
(defconst sample-get-group-list-response (test-helper-json-resource "azure-group-list-response.json"))

(ert-deftest ecloud-group-test--ecloud-fetch-resources ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure group)
   (ecloud-parse-resource-data sample-get-group-list-response 'azure-group)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "group"))
   (should (has-resource azure-group (("name" "default")
                                      ("id"  "/subscriptions/dead3122-ecd3-dead-beef-aaaabbbbfacf/resourcegroups/default"))))))

(defconst azure-group-list-view-result
  (s-trim-left "
azure group
name        location   state         
default     eastus     Succeeded"))

(ert-deftest ecloud-group-test--ecloud-insert-list-views ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure group)
   (ecloud-parse-resource-data sample-get-group-list-response 'azure-group)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(group))
       (should (equal azure-group-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))))))


(provide 'ecloud-group-test)

;;; ecloud-group-test.el ends here
