;;; ecloud-state-test.el --- Tests for ecloud state.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(load-file "./test/test-helper.el")

(require 'dash)
(require 'magit)
(require 'ecloud-state)
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

(describe "Wen registering resource type"
          (before-each
           (ecloud-register-resource-type "azure" "vnet"))

          (it "state for the cloud should be initialized"
              (expect (ht-size (ecloud-state)) :to-equal 1)
              (expect (ht-get (ecloud-state) "azure") :not :to-equal nil))

          (it "Resource type for the cloud should be initialized"
              (expect (ecloud-get-resource-type-state "azure" "vnet") :not :to-equal nil)))
