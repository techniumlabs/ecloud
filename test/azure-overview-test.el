;;; azure-overview-test.el --- Tests for azure overview.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(require 'ecloud-utils)

(declare-function test-helper-json-resource "test-helper.el")

(ert-deftest ecloud-azure-overview-test--azure-overview-buffer-created ()
  (test-helper-with-empty-state
   (cl-letf (((symbol-function 'ecloud-fetch-resources) (lambda (class &rest args) "")))
     (call-interactively 'azure-overview)
     (should (ecloud-mode-get-buffer 'azure-overview-mode))
     )
   ))
