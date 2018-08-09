;;; azure-overview.el --- Overview Buffer for AZURE -*- lexical-binding: t -*-

;; Author: Ramanathan Sivagurunathan
;; Maintainer: Ramanathan Sivagurunathan
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (magit "2.8.0"))
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; TODO Add commentary

;;; Code:
(require 'magit-popup)
(require 'ecloud-vars)
(require 'ecloud-view)
(require 'azure-vars)
(require 'azure-account)
(require 'azure-vm)
(require 'azure-resource-group)

(defcustom azure-overview-list-views
  '(account group)
  "Components that are part of the azure overview view"
  :package-version '(ecloud . "0.0.1")
  :group 'ecloud)

(magit-define-popup azure-overview-dispatch-popup
  "Popup console for showing an overview of available popup commands."
  :group 'ecloud
  :actions
  '("Popup and dwim commands"
    (?v "virtual machines" azure-vm-popup)
    (?a "Account" azure-account-popup)
    (?g "Resource Group" azure-group-popup)))

(defcustom azure-overview-sections-hook
  '(azure-insert-overview-headers
    azure-insert-views)
  "Hook run to insert sections into a status buffer."
  :package-version '(ecloud . "0.0.1")
  :group 'ecloud
  :type 'hook)

(defun azure-insert-overview-headers ()
  "Insert header sections appropriate for `magit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`magit-status-headers-hook'."
  (insert "Azure Cloud\n\n")
  )

(defun azure-insert-views ()
  "Insert the defined views"
  (ecloud-insert-list-views 'azure azure-overview-list-views))

(defun azure-overview-refresh-buffer ()
  ;; Trigger Refresh data
  (--map (let ((rtype (intern (format "azure-%s" it))))
           (ecloud-fetch-resources rtype))
        azure-overview-list-views)
  (magit-insert-section (status)
    (magit-run-section-hook 'azure-overview-sections-hook)))

(defun azure-overview-print-section (&optional intent)
  "Add the change at point to the staging area.
With a prefix argument, INTENT, and an untracked file (or files)
at point, stage the file but not its content."
  (interactive "P")
  (let* ((section (magit-current-section))
         (type (oref section type))
         (value (oref section value)))
    (message (format "%s" value))
    )
)

(defvar azure-overview-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;;TODO
    (define-key keymap (kbd "h") 'azure-overview-dispatch-popup)
    keymap)
  "Keymap for `azure-overview-mode'.")

;;;###autoload
;; TODO Update
(define-derived-mode azure-overview-mode ecloud-mode "Azure Overview"
  "Mode for working with azure overview."
  :group 'ecloud)

;;;###autoload
(defun azure-overview ()
  "Display an overview buffer for azure"
  (interactive)
  (azure-overview-internal default-directory))

(defun azure-overview-internal (directory)
  (magit--tramp-asserts directory)
  (let ((default-directory directory))
    (magit-mode-setup #'azure-overview-mode)))

(provide 'azure-overview)

;;; azure-overview.el ends here
