;;; gcp-overview-test.el --- Tests for gcp overview.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'gcp-overview)

(describe "Gcp Overview"
          (describe "When triggered"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model gcp project)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "gcp-project-list-response.json")
                                    'gcp-project)
                                   ))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (run-hooks 'gcp-overview-sections-hook)
                            (expect (s-trim  "Gcp Cloud

gcp project
name             lifecycleState     
gcloud-start     ACTIVE             
gcloud-end       ACTIVE             

No Errors") :to-match
(s-trim (substring-no-properties (buffer-string)))))))
                    )
          )

