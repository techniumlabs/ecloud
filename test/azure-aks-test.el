;;; azure-aks-test.el --- Tests for azure aks.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(require 'ecloud-utils)

(declare-function test-helper-json-resource "test-helper.el")
(defconst sample-get-aks-list-response (test-helper-json-resource "azure-aks-list-response.json"))

(ert-deftest ecloud-aks-test--ecloud-fetch-resources ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure aks)
   (ecloud-parse-resource-data sample-get-aks-list-response 'azure-aks)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "aks"))
   (should (has-resource azure-aks (("name" "gnuherd")
                                    ("id" "/subscriptions/deadbeef-beed-bade-b70f-ee9206029c60/resourcegroups/gnuherd/providers/Microsoft.ContainerService/managedClusters/gnuherd"))))))

(ert-deftest ecloud-aks-test--ecloud-fetch-resources-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure aks)
   (ecloud-parse-resource-data nil 'azure-aks)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "azure"))
   (should (ht-get (ht-get (ecloud-state) "azure") "aks"))
   (should (equal (ecloud-resource-count azure-aks) 0))
   ))

(defconst azure-aks-list-view-result
  (s-trim-left "
azure aks
name        kubernetesVersion     location          size     provisioningState     
gnuherd     1.10.6                australiaeast     1        Succeeded"))

(ert-deftest ecloud-aks-test--ecloud-insert-list-views ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure aks)
   (ecloud-parse-resource-data sample-get-aks-list-response 'azure-aks)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(aks))
       (should (equal azure-aks-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(defconst azure-aks-list-view-empty-result
  (s-trim-left "
azure aks
No aks found"))

(ert-deftest ecloud-aks-test--ecloud-insert-list-views-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure aks)
   (ecloud-parse-resource-data nil 'azure-aks)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(aks))
       (should (equal azure-aks-list-view-empty-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(ert-deftest ecloud-aks-test--azure-aks-scale ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model azure aks)
   (ecloud-parse-resource-data sample-get-aks-list-response 'azure-aks)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'azure '(aks))
       (should (equal azure-aks-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))
       (goto-line 3)
       (cl-letf (((symbol-function 'ecloud-read-int) (lambda (prompt &rest args) 10))
                 ((symbol-function 'magit-confirm) (lambda (action &rest args) t))
                 ((symbol-function 'ecloud-run-command) (lambda (cmd args on-success &rest args) t))
                 )
         (call-interactively 'azure-aks-scale)
         (should (equal azure-aks-list-view-result
                        (s-trim (substring-no-properties (buffer-string))))))
       ))))

(provide 'ecloud-aks-test)

;;; azure-aks-test.el ends here
