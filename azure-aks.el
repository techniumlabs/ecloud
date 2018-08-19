;;; azure-aks.el --- Azure Aks.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 The Ecloud Contributors

;; Author: Ramanathan Sivagurunathan <ramzthecoder+ecloud@gmail.com>

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (magit "2.13.0"))

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
;; TODO Add commentary
;;; Code:

(require 'ecloud-crud)
(require 'ecloud-state)
(require 'ecloud-utils)
(require 'eieio)
(eval-when-compile (require 'cl))

(defvar azure-aks--list-command
  '("az" "aks" "list")
  "Azure cli for getting aks list")

(defvar azure-aks-list-view-display-params
  '(name kubernetesVersion location size provisioningState)
  "List of attributes to display in list view")

;; Model for Azure Aks
(ecloud-define-resource-model azure aks)

(defcustom azure-aks-parser-hook
  '(azure-aks--parse-node-pool-size)
  "Hook to run for parsing json data."
  :group 'ecloud-azure
  :type 'hook)

;; Parse the provisioning state
(defun azure-aks--parse-node-pool-size (robj)
  (-let* (((&alist 'agentPoolProfiles [(&alist 'count nodepool-size)]) (oref robj :attributes))
          (nodepool-size (number-to-string nodepool-size)))
    (oset robj :attributes (append (oref robj :attributes) `((size . ,nodepool-size))))
    ))

(cl-defun azure-aks-scale ()
  (interactive)
  (let* ((section (magit-current-section))
         (type (oref section type))
         (value (oref section value))
         (aks-name (oref value :name))
         (aks-group (ecloud-get-attributes value 'resourceGroup))
         (node-count (number-to-string (magit-read-int (format "Scale the aks cluster %s to"
                                              aks-name)))))
    (if (magit-confirm t (format "Do you want to scale aks cluster %s to %s"
                                 aks-name node-count))
        (ecloud-run-json-command `("az" "aks" "scale"
                                   "--name" ,aks-name
                                   "--resource-group" ,aks-group
                                   "--node-count" ,node-count)
                                 ()
                                 (lambda (json-output)
                                   (message "%s" json-output))))))

(ecloud-define-simple-resource-action azure-aks-browse
                                      ("az" "aks" "browse" "--name" name "--resource-group" resourceGroup))

(magit-define-popup azure-aks-popup
  "Popup console for ask commands."
  :group 'ecloud
  :actions
  '((?s "Scale" azure-aks-scale)
    (?b "Browse" azure-aks-browse))
  :max-action-columns 3)

(defvar magit-azure-aks-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'azure-overview-print-section)
    (define-key map "h" 'azure-aks-popup)
    map)
  "Keymap for the `azure-aks' section.")

(provide 'azure-aks)
;;; azure-aks.el ends here
