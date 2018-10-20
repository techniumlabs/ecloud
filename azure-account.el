;;; azure-account.el --- Azure Account.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 The Ecloud Contributors

;; Author: Ramanathan Sivagurunathan <ramzthecoder+ecloud@gmail.com>

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (magit "2.13.0") (ht "2.2") (s "1.12.0") (pcache "0.4.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Contains code to handle azure account

;;; Code:

(require 'ecloud-model)
(require 'ecloud-state)
(require 'ecloud-view)
(require 'ecloud-mode)
(require 'ecloud-utils)
(require 'magit)
(require 'subr-x)
(require 'eieio)
(eval-when-compile (require 'cl))

(defvar azure-account--list-command
  '("az" "account" "list")
  "Azure cli for getting account list.")

(defvar azure-account-list-view-display-params
  '(name state isDefault)
  "List of attributes to display in list view.")

;; Model for Azure Account
(ecloud-define-resource-model azure account)

;; View for Azure Account
(ecloud-setup-resource-view azure account)

;;;; Hooks
(defcustom azure-account-detailed-view-hook
  '(azure-account-add-detailed-view-section)
  "Hook to run for adding a detailed view."
  :group 'ecloud-azure
  :type 'hook)

;;;; View Functions
(defun azure-account-add-detailed-view-section (robj)
  "Function to add detailed view section for the account.`ROBJ is the object for the account."
  (-let (((&alist 'cloudName cloudName 'tenantId tenantId 'user (&alist 'name username 'type usertype)) (oref robj attributes)))
    (insert ?\n)

    (ecloud-insert-kv-list `(("Cloud Name" . ,cloudName)
                             ("Tenant Id" . ,tenantId)
                             ("Username" . ,username)
                             ("User Type" . ,usertype)))
    (insert ?\n)))

;;; Actions
(ecloud-define-cautious-action azure-account-set-default
                               ("az" "account" "set" "--subscription" id)
                               ("Do you want to set %s to be current" name))

(defvar magit-azure-account-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" 'azure-account-popup)
    (define-key map "s" 'azure-account-set-default)
    (define-key map [tab] 'magit-section-cycle)
    map)
  "Keymap for the `azure-account' section.")

(magit-define-popup azure-account-popup
  "Popup console for azure account"
  'ecloud
  :actions
  '("Azure account commands"
    (?s "Set current" azure-account-set-default)))

(provide 'azure-account)
;;; azure-account.el ends here
