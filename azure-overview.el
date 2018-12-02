;;; azure-overview.el --- Overview Buffer for AZURE -*- lexical-binding: t -*-

;; Author: Ramanathan Sivagurunathan
;; Maintainer: Ramanathan Sivagurunathan
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (magit "2.13.0") (ht "2.2") (s "1.12.0") (pcache "0.4.2"))
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
;; Contains code to display the overview page for azure.

;;; Code:
(require 'magit-popup)
(require 'ecloud-vars)
(require 'ecloud-view)
(require 'azure-vars)
(require 'azure-account)
(require 'azure-vnet)
(require 'azure-nsg)
(require 'azure-routetable)
(require 'azure-tag)
(require 'azure-aks)
(require 'azure-vm)
(require 'azure-storage-account)
(require 'azure-group)
(require 'azure-appservice-plan)
(require 'azure-public-ip)
(require 'azure-lb)
(require 'azure-acr)
(require 'azure-aci)

(defcustom azure-overview-list-views
  '(account vnet vm aks acr aci)
  "Components that are part of the azure overview view."
  :package-version '(ecloud . "0.0.1")
  :group 'ecloud
  :type 'list)

(magit-define-popup azure-overview-dispatch-popup
  "Popup console for showing an overview of available popup commands."
  'ecloud
  :actions
  '("Popup and dwim commands"
    (?a "Account" azure-account-overview)
    (?g "Resource Group" azure-group-overview)
    (?v "virtual machines" azure-vm-overview)
    (?V "vnet" azure-vnet-overview)))

(defcustom azure-overview-sections-hook
  '(azure-insert-overview-headers
    azure-insert-views
    azure-insert-error-view
    )
  "Hook run to insert sections into a status buffer."
  :package-version '(ecloud . "0.0.1")
  :group 'ecloud
  :type 'hook)

(defun azure-insert-overview-headers ()
  "Insert header sections appropriate for `magit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`magit-status-headers-hook'."
  (insert (propertize "Azure Cloud\n\n" 'face 'ecloud-cloud-title)))

(defun azure-insert-views ()
  "Insert the defined views."
  (ecloud-insert-list-views 'azure azure-overview-list-views))

(defun azure-insert-error-view ()
  "Insert the error view."
  (ecloud-insert-error-view 'azure))

(defun azure-overview-refresh-buffer ()
  "Function to refresh the overview buffer."
  (interactive)
  ;; Trigger Refresh data
  (azure-overview-refresh-view)
  (--map (let ((rtype (intern (format "azure-%s" it))))
           (ecloud-fetch-resources rtype)) azure-overview-list-views))

(defun azure-overview-refresh-view ()
  "Refresh the overview view."
  (magit-insert-section (status)
    (magit-run-section-hook 'azure-overview-sections-hook)))

(defvar azure-overview-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;;TODO
    (define-key keymap (kbd "?") 'azure-overview-dispatch-popup)
    (define-key keymap (kbd "r") 'azure-overview-refresh-buffer)
    keymap)
  "Keymap for `azure-overview-mode'.")

;;;###autoload
(define-derived-mode azure-overview-mode ecloud-mode "Azure Overview"
  "Mode for working with azure overview."
  :group 'ecloud)

;;;###autoload
(defun azure-overview ()
  "Display an overview buffer for azure."
  (interactive)
  (azure-overview-internal))

(defun azure-overview-internal ()
  "Internal function for displaying overview buffer."
  (ecloud-mode-setup #'azure-overview-mode))

(provide 'azure-overview)
;;; azure-overview.el ends here
