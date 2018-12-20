;;; aws-ecr-test.el --- Tests for aws ecr.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'aws-ecr)


(describe "Aws Ecr"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model aws ecr)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "aws-ecr-list-response.json")
                                    'aws-ecr)))

                    (it "Should return correct name and id"
                        (expect (has-resource aws-ecr (("name" "testrepo")
                                                       ))
                                :not :to-be nil)
                        (expect (ecloud-resource-count aws-ecr) :to-be 1))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'aws '(ecr))
                            (expect (s-trim "aws ecr
name         repositoryuri                                                  
testrepo     123456787909.dkr.ecr.ap-southeast-2.amazonaws.com/testrepo") :to-match
(s-trim (substring-no-properties (buffer-string)))))))
                    )

          (describe "When Parsing a null response"

                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model aws ecr)
                                   (ecloud-parse-resource-data
                                    (json-read-from-string "{\"Ecrs\": []}")
                                    'aws-ecr)))
                    (it "Should not have any entries"
                        (expect (ecloud-resource-count aws-ecr) :to-be 0))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'aws '(ecr))
                            (expect (s-trim  "aws ecr
No ecr found") :to-match
(s-trim (substring-no-properties (buffer-string))))))))
          )
