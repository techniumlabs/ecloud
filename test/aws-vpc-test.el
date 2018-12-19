;;; aws-vpc-test.el --- Tests for aws vpc.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'aws-vpc)


(describe "Aws Vpc"
          (describe "When Parsing a valid response"
                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model aws vpc)
                                   (ecloud-parse-resource-data
                                    (test-helper-json-resource "aws-vpc-list-response.json")
                                    'aws-vpc)))

                    (it "Should return correct name and id"
                        (expect (has-resource aws-vpc (("name" "Default")
                                                       ))
                                :not :to-be nil)
                        (expect (ecloud-resource-count aws-vpc) :to-be 2))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'aws '(vpc))
                            (expect (s-trim "aws vpc
name        CidrBlock         IsDefault     
Default     172.31.0.0/16     true          
test        10.0.0.0/16       false") :to-match
(s-trim (substring-no-properties (buffer-string)))))))
                    )

          (describe "When Parsing a null response"

                    (before-each (progn
                                   (ecloud-state-init)
                                   (ecloud-define-resource-model aws vpc)
                                   (ecloud-parse-resource-data
                                    (json-read-from-string "{\"Vpcs\": []}")
                                    'aws-vpc)))
                    (it "Should not have any entries"
                        (expect (ecloud-resource-count aws-vpc) :to-be 0))

                    (it "Should correctly display view"
                        (with-temp-buffer
                          (save-excursion
                            (ecloud-insert-list-views 'aws '(vpc))
                            (expect (s-trim  "aws vpc
No vpc found") :to-match
(s-trim (substring-no-properties (buffer-string))))))))
          )
