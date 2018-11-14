;;; ecloud-model-test.el --- Tests for ecloud model.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'dash)
(require 'magit)
(require 'ecloud-model)
(require 'buttercup)


(describe "Resource Model when defined"

          (it "should create a instance of class"
              (ecloud-define-resource-model azure vnet)
              (expect (make-instance 'azure-vnet) :not :to-equal nil)))

(describe "Resource Model Instance"
          :var (resource-instance)
          (before-each
           (ecloud-define-resource-model azure vnet)
           (setq resource-instance (make-instance 'azure-vnet
                                                  :name "test"
                                                  :id "test-id"
                                                  :has (list (cons "azure-subnet" (list "test-subnet-0"
                                                                                        "test-subnet-1")))
                                                  :belongs-to (list (cons "azure-resource-group" (list "test-rg-1"))))
                 ))

          (it "Should have a name if defined"
              (expect (ecloud-resource-name resource-instance) :to-equal "test"))

          (it "Should have an id if defined"
              (expect (ecloud-resource-id resource-instance) :to-equal "test-id"))

          (it "Should have a type if defined"
              (expect (ecloud-resource-type resource-instance) :to-equal "azure-vnet"))

          (it "Should give all associated resource type it has"
              (expect (ecloud-resource-has-type resource-instance) :to-equal '("azure-subnet")))

          (it "Should give all associated resource type it belongs to"
              (expect (ecloud-resource-belongs-to-type resource-instance) :to-equal '("azure-resource-group")))

          (it "Should give all associated resource of type it has"
              (expect (ecloud-resource-has resource-instance "azure-subnet") :to-equal (list "test-subnet-0"
                                                                                             "test-subnet-1") ))

          (it "Should give all associated resource of type it belongs to"
              (expect (ecloud-resource-belongs-to resource-instance "azure-resource-group") :to-equal (list "test-rg-1")))

          (it "Should add new association for existing type"
              (ecloud-resource-add-has resource-instance "azure-subnet" "test-subnet-2")
              (expect (ecloud-resource-has resource-instance "azure-subnet") :to-have-same-items-as (list "test-subnet-0" "test-subnet-1" "test-subnet-2"))
              )

          (it "Should add new association it has for new type"
              (ecloud-resource-add-has resource-instance "azure-route-table" "test-rt-0")
              (ecloud-resource-add-has resource-instance "azure-route-table" "test-rt-1")
              (expect (ecloud-resource-has resource-instance "azure-route-table") :to-have-same-items-as (list "test-rt-0" "test-rt-1"))
              )

          (it "Should remove any existing association it has"
              (ecloud-resource-delete-has resource-instance "azure-subnet" "test-subnet-1")
              (expect (ecloud-resource-has resource-instance "azure-subnet") :to-equal (list "test-subnet-0"))
              (ecloud-resource-delete-has resource-instance "azure-subnet" "test-subnet-0")
              (expect (ecloud-resource-has resource-instance "azure-subnet") :to-equal nil))

          (it "Should add new belongs to association for already existing type"
              (ecloud-resource-add-belongs-to resource-instance "azure-resource-group" "test-rg-0")
              (expect (ecloud-resource-belongs-to resource-instance "azure-resource-group") :to-have-same-items-as (list "test-rg-0" "test-rg-1")))

          (it "Should add new belongs to association for new type"
              (ecloud-resource-add-belongs-to resource-instance "azure-account" "test-account-0")
              (ecloud-resource-add-belongs-to resource-instance "azure-account" "test-account-1")
              (expect (ecloud-resource-belongs-to resource-instance "azure-account") :to-have-same-items-as (list "test-account-0" "test-account-1")))

          (it "Should remove any existing belongs to association"
              (ecloud-resource-delete-belongs-to resource-instance "azure-resource-group" "test-rg-1")
              (expect (ecloud-resource-belongs-to resource-instance "azure-resource-group") :to-equal nil))
          )
