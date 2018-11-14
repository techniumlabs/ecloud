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

(describe "State on initialization"
          (before-each
           (ecloud-state-init))
          
          (it "should be empty"
              (expect (ht-size (ecloud-state)) :to-equal 0))

          (it "Should have a cache directory created"
              (expect (f-dir? (format "%s/ecloud" pcache-directory)))))

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

(describe "On Fetching"
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

          (it "For a resource name should give all resource for that type and name"
              (expect (length (ecloud-state--get-resource-by-name "azure" "vnet" "aci-test")) :to-equal 1))

          (it "For a resource type that belongs to a parent should give all the resources that it belongs to"
              (expect (length (ecloud-state--get-all-resource-belonging-to "azure" "subnet" (first (ecloud-state--get-resource-by-name "azure" "vnet" "aci-test")))) :to-equal 1)
              (expect (length (ecloud-state--get-all-resource-belonging-to "azure" "subnet" (first (ecloud-state--get-resource-by-name "azure" "vnet" "pegasus2-vnet")))) :to-equal 2))

          )
