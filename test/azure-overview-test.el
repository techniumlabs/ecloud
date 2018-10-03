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
   (azure-overview)
   (should (ecloud-mode-get-buffer 'azure-overview-mode))
   ))
