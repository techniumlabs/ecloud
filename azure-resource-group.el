;;; azure-group.el --- Azure Group.  -*- lexical-binding: t; -*-

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
;; TODO Add commentary
;;; Code:

(require 'ecloud-crud)
(require 'ecloud-state)
(require 'eieio)
(eval-when-compile (require 'cl))

(defvar azure-group--list-command '("az" "group" "list"))
(defvar azure-group-list-view-display-params '(name location state))

;; Model for Azure Group
(ecloud-define-resource-model azure group)

;; View for Azure Group
(ecloud-setup-resource-view azure group)

(defcustom azure-group-parser-hook
  '(azure-group--parse-provisioning-state)
  "Hook to run for parsing json data."
  :group 'ecloud-azure
  :type 'hook)

;; Parse the provisioning state
(defun azure-group--parse-provisioning-state (robj)
  (-let (((&alist 'properties (&alist 'provisioningState state)) (oref robj :attributes)))
    (oset robj :attributes (append (oref robj :attributes) `((state . ,state))))))

;;;; Actions
(ecloud-define-cautious-action azure-group-delete-group
                                      ("az" "group" "delete" "--name" name "--yes" "--output" "json")
                                      ("Do you want to delete group %s" name ))

(defvar magit-azure-group-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'azure-group-delete-group)
    map)
  "Keymap for the `azure-group' section.")

(provide 'azure-resource-group)
;;; azure-group.el ends here
