;;; azure-public-ip-test.el --- Tests for azure public ip.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'buttercup)
(require 'dash)
(require 'magit)
(require 'azure-public-ip)


(describe "When Parsing Azure public ip address"
          :var (sample-get-public-ip-list-response)
          (before-each (progn
                         (ecloud-state-init)
                         (ecloud-define-resource-model azure public-ip)
                         (ecloud-parse-resource-data (test-helper-json-resource "azure-public-ip-list-response.json") 'azure-public-ip)))

          (it "Returns correct name and id"
              (expect (has-resource azure-public-ip (("name" "technium-ip")
                                                     ("id" "/subscriptions/12345678-73a9-44e3-b70f-ee9206029c60/resourcegroups/technium/providers/microsoft.network/publicipaddresses/technium-ip"))) :not :to-be nil)))

