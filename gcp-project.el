;;; gcp-project.el --- Gcp Project.  -*- lexical-binding: t; -*-

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

;;;; Commentary:
;; Contains code to parse and display gcp projects.

;;; Code:

(require 'ecloud-model)
(require 'ecloud-state)
(require 'eieio)
(eval-when-compile (require 'cl))

(defvar gcp-project--list-command
  '("gcloud" "projects" "list" "--format" "json")
  "Gcp cli for getting project list.")

(defvar gcp-project-list-view-display-params
  '(name lifecycleState)
  "List of attributes to display in list view.")

;; Model for Gcp Project
(ecloud-define-resource-model gcp project)

;;;; Actions

(defvar magit-gcp-project-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" 'gcp-project-popup)
    map)
  "Keymap for the section `gcp-project.")

(magit-define-popup gcp-project-popup
  "Popup console for gcp project"
  'ecloud
  :actions
  '("Gcp project commands"
    (?s "Set current" gcp-project-set-default)))

(provide 'gcp-project)
;;; gcp-project.el ends here
