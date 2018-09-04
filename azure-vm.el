;;; azure-vm.el --- Azure Account.  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ecloud-crud)
(require 'ecloud-state)
(require 'eieio)
(eval-when-compile (require 'cl))

(defvar azure-vm--list-command
  '("az" "vm" "list" "-d")
  "Azure cli for getting vm list")

(defvar azure-vm-list-view-display-params
  '(name location)
  "List of attributes to display in list view")

;; Model for Azure Vm
(ecloud-define-resource-model azure vm)

;; View for Azure Vm
(ecloud-setup-resource-view azure vm)

(ecloud-define-simple-resource-action azure-vm-start
                                      ("az" "vm" "start" "--name" name "--resource-group" resourceGroup "--no-wait"))

(ecloud-define-cautious-action azure-vm-stop
                               ("az" "vm" "stop" "--name" name "--resource-group" resourceGroup "--no-wait")
                               ("Do you want to stop vm %s" name ))


(defvar magit-azure-vm-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'azure-overview-print-section)
    (define-key map "s" 'azure-vm-start)
    (define-key map "S" 'azure-vm-stop)
    map)
  "Keymap for the `azure-vm' section.")

(provide 'azure-vm)
;;; azure-vm.el ends here
