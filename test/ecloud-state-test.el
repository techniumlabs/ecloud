;;; ecloud-state-test.el --- Tests for application state management.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'dash)
(require 'ecloud-state)
(declare-function test-helper-json-resource "test-helper.el")

(ert-deftest ecloud-state-test--ecloud-register-cloud ()
  (test-helper-with-empty-state
    (ecloud-register-cloud "azure")
    (should ecloud-state--current-state)
    (should (ht-get (ecloud-state) "azure"))))


(ert-deftest ecloud-state-test--ecloud-register-resource ()
  (test-helper-with-empty-state
    (ecloud-register-resource "azure" "account")
    (should ecloud-state--current-state)
    (should (ht-get (ecloud-state) "azure"))
    (should (ht-get (ht-get (ecloud-state) "azure") "account"))))

(provide 'ecloud-state-test)

;;; ecloud-state-test.el ends here
