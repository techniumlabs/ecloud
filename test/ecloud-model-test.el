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
                                                  :has '((azure-subnet (list "test-subnet-1")))
                                                  :belongs-to '((azure-resource-group (list "test-rg"))))
                 ))

          (it "Should have a name if defined"
              (expect (ecloud-resource-name resource-instance) :to-equal "test"))

          (it "Should have an id if defined"
              (expect (ecloud-resource-id resource-instance) :to-equal "test-id"))

          (it "Should give all associated resource type it has"
              (expect (ecloud-resource-has-type resource-instance) :to-equal '(azure-subnet)))

          (it "Should give all associated resource type it belongs to"
              (expect (ecloud-resource-belongs-to-type resource-instance) :to-equal '(azure-resource-group)))
          )
