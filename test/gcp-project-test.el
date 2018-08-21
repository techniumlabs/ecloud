;;; gcp-project-test.el --- Tests for gcp project.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(declare-function test-helper-json-resource "test-helper.el")
(defconst sample-get-project-list-response (test-helper-json-resource "gcp-project-list-response.json"))

(ert-deftest ecloud-project-test--ecloud-fetch-resources ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model gcp project)
   (ecloud-parse-resource-data sample-get-project-list-response 'gcp-project)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "gcp"))
   (should (ht-get (ht-get (ecloud-state) "gcp") "project"))
   (should (has-resource gcp-project (("name" "gcloud-start"))))))

(ert-deftest ecloud-project-test--ecloud-fetch-resources-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model gcp project)
   (ecloud-parse-resource-data nil 'gcp-project)
   (should ecloud-state--current-state)
   (should (ht-get (ecloud-state) "gcp"))
   (should (ht-get (ht-get (ecloud-state) "gcp") "project"))
   (should (equal (ecloud-resource-count gcp-project) 0))
   ))

(defconst gcp-project-list-view-result
  (s-trim-left "
gcp project
name             lifecycleState     
gcloud-start     ACTIVE             
gcloud-end       ACTIVE"))

(ert-deftest ecloud-project-test--ecloud-insert-list-views ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model gcp project)
   (ecloud-parse-resource-data sample-get-project-list-response 'gcp-project)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'gcp '(project))
       (should (equal gcp-project-list-view-result
                      (s-trim (substring-no-properties (buffer-string)))))))))

(defconst gcp-project-list-view-empty-result
  (s-trim-left "
gcp project
No project found"))

(ert-deftest ecloud-project-test--ecloud-insert-list-views-empty ()
  (test-helper-with-empty-state
   (ecloud-define-resource-model gcp project)
   (ecloud-parse-resource-data nil 'gcp-project)
   (should ecloud-state--current-state)
   (with-temp-buffer
     (save-excursion
       (ecloud-insert-list-views 'gcp '(project))
       (should (equal gcp-project-list-view-empty-result
                      (s-trim (substring-no-properties (buffer-string)))))))))


(provide 'gcp-project-test)

;;; gcp-project-test.el ends here
