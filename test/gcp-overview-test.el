;;; gcp-overview-test.el --- Tests for gcp overview.  -*- lexical-binding: t; -*-
;;; Commentary:
;;;; Code:

(require 'dash)
(require 'ecloud-state)
(require 'ecloud-crud)
(require 'ecloud-utils)

(declare-function test-helper-json-resource "test-helper.el")

(ert-deftest ecloud-gcp-overview-test--gcp-overview-buffer-created ()
  (test-helper-with-empty-state
   (cl-letf (((symbol-function 'ecloud-fetch-resources) (lambda (class &rest args) "")))
     (call-interactively 'gcp-overview)
     (should (ecloud-mode-get-buffer 'gcp-overview-mode))
     )))
