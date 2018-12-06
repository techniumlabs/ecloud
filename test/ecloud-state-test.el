;;; ecloud-state-test.el --- Tests for ecloud state.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'dash)
(require 'magit)
(require 'ecloud-state)
(require 'ecloud-model)
(require 'azure-vnet)
(require 'buttercup)

(describe "State"
          (before-each
           (setq ecloud-state--current-state nil)
           (delete-directory (format "%s/ecloud" pcache-directory) t))

          (it "should be initialized on first access"
              (expect (ht-size (ecloud-state)) :to-equal 0)))

(describe "State on initialization"
          (before-each
           (setq ecloud-state--current-state nil)
           (delete-directory (format "%s/ecloud" pcache-directory) t)
           (ecloud-state-init))
          
          (it "should be empty"
              (expect (ht-size (ecloud-state)) :to-equal 0))

          (it "Should have a cache directory created"
              (expect (f-dir? (format "%s/ecloud" pcache-directory))))

          (it "Should retrieve from cache"
              (ecloud-define-resource-model azure account)
              (ecloud-parse-resource-data
               (test-helper-json-resource "azure-account-list-response.json")
               'azure-account)
              (expect (ecloud-resource-count azure-account) :to-be 2)
              (make-directory (format "%s/ecloud/azure" pcache-directory))
              (pcache-save (pcache-repository "ecloud/azure/account") t)
              (ecloud-state-init)
              (ecloud-define-resource-model azure account)
              (expect (ecloud-resource-count azure-account) :to-be 2)
              ))

(describe "When registering cloud"
          (before-each
           (ecloud-register-cloud "azure"))

          (it "state for the cloud should be initialized"
              (expect (ht-size (ecloud-state)) :to-equal 1)
              (expect (ecloud-get-cloud-state "azure") :not :to-equal nil)))

(describe "When registering resource type"
          (before-each
           (ecloud-register-resource-type "azure" "vnet"))

          (it "state for the cloud should be initialized"
              (expect (ht-size (ecloud-state)) :to-equal 1)
              (expect (ht-get (ecloud-state) "azure") :not :to-equal nil))

          (it "Resource type for the cloud should be initialized"
              (expect (ecloud-get-resource-type-state "azure" "vnet") :not :to-equal nil)))

(describe "Errors"
          (it "should be able to added by cloud"
              (ecloud-state-add-error "azure" "Failed" "More Details")
              (expect (ecloud-state-get-errors "azure") :to-equal '(("Failed" . "More Details")))))

(describe "When retrieving resource details from server"
          (before-each
           (ecloud-state-init))

          (spy-on 'ecloud-run-json-command)
          
          (it "Should be able to call external process with right arguments"
              (ecloud-fetch-resources "azure-account")
              (expect (nth 0 (spy-calls-args-for 'ecloud-run-json-command 0)) :to-equal '("az" "account" "list"))
              (expect (nth 1 (spy-calls-args-for 'ecloud-run-json-command 0)) :to-equal nil))
          )

(describe "When querying for resource details"
          (before-each
           (ecloud-state-init)
           (ecloud-parse-resource-data (test-helper-json-resource "azure-vnet-list-response.json") 'azure-vnet))

          (it "For a resource type should give all resources for the type"
              (expect (length (ecloud-state--get-all-resource-type "azure" "vnet")) :to-equal 7)
              (expect (--map  (ecloud-resource-attribute it "name")
                              (ecloud-state--get-all-resource-type "azure" "vnet"))
                      :to-have-same-items-as
                      (list "myVMVNET"
                            "vnodes-vnet"
                            "vnode2-vnet"
                            "vnode-preview-vnet"
                            "pegasus2-vnet"
                            "aci-test"
                            "aks-vnet-deadbeef")))

          (it "when updated twice Should be added once"
              (ecloud-parse-resource-data (test-helper-json-resource "azure-vnet-list-response.json") 'azure-vnet)
              (expect (length (ecloud-state--get-all-resource-type "azure" "vnet")) :to-equal 7)
              (expect (--map  (ecloud-resource-attribute it "name")
                              (ecloud-state--get-all-resource-type "azure" "vnet"))
                      :to-have-same-items-as
                      (list "myVMVNET"
                            "vnodes-vnet"
                            "vnode2-vnet"
                            "vnode-preview-vnet"
                            "pegasus2-vnet"
                            "aci-test"
                            "aks-vnet-deadbeef"))
              )

          (it "For a resource name should give all resource for that type and name"
              (expect (length (ecloud-state--get-resource-by-name "azure" "vnet" "aci-test")) :to-equal 1))

          (it "For a resource type that belongs to a parent should give all the resources that it belongs to"
              (expect (length (ecloud-state--get-all-resource-belonging-to "azure" "subnet" (first (ecloud-state--get-resource-by-name "azure" "vnet" "aci-test")))) :to-equal 1)
              (expect (length (ecloud-state--get-all-resource-belonging-to "azure" "subnet" (first (ecloud-state--get-resource-by-name "azure" "vnet" "pegasus2-vnet")))) :to-equal 2))

          )
