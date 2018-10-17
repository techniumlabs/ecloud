;;; azure-acr.el --- Azure Container Registry.  -*- lexical-binding: t; -*-

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

;;;; Code:

(require 'ecloud-crud)
(require 'ecloud-state)
(require 'eieio)
(eval-when-compile (require 'cl))

(defvar azure-acr--list-command
  '("az" "acr" "list")
  "Azure cli for getting acr list")

(defvar azure-acr-list-view-display-params
  '(name loginServer location)
  "List of attributes to display in list view")

;; Model for Azure Container Registry
(ecloud-define-resource-model azure acr)

;; View for Azure Container Registry
(ecloud-setup-resource-view azure acr)

(defvar magit-azure-acr-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'azure-overview-print-section)
    map)
  "Keymap for the `azure-acr' section.")

(provide 'azure-acr)
;;; azure-acr.el ends here
