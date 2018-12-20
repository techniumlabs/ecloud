;;; aws-ecr.el --- Aws Ecr.  -*- lexical-binding: t; -*-

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
;; Contains code to handle aws ecr

;;; Code:

(require 'ecloud-model)
(require 'ecloud-state)
(require 'ecloud-view)
(require 'ecloud-mode)
(require 'magit)
(require 'subr-x)
(require 'eieio)
(eval-when-compile (require 'cl))

(defvar aws-ecr--name-attribute 'repositoryName
  "Name of name attribute in json.")

(defvar aws-ecr--list-command
  '("aws" "ecr" "describe-repositories")
  "Aws cli for getting ecr list.")

(defvar aws-ecr-list-view-display-params
  '(name repositoryUri)
  "List of attributes to display in list view.")

;; Model for Aws Ecr
(ecloud-define-resource-model aws ecr)

;; View for Aws Ecr
(ecloud-setup-resource-view aws ecr)

;; Prepare data
(defun aws-ecr--prepare-data (data)
  "Function to prepare the `DATA for easier parsing."
  (-let (((&alist 'repositories ecrlist) data))
    ecrlist))

(provide 'aws-ecr)
;;; aws-ecr.el ends here
