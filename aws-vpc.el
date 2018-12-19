;;; aws-vpc.el --- Aws Vpc.  -*- lexical-binding: t; -*-

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
;; Contains code to handle aws vpc

;;; Code:

(require 'ecloud-model)
(require 'ecloud-state)
(require 'ecloud-view)
(require 'ecloud-mode)
(require 'magit)
(require 'subr-x)
(require 'eieio)
(eval-when-compile (require 'cl))

(defvar aws-vpc--list-command
  '("aws" "ec2" "describe-vpcs")
  "Aws cli for getting vpc list.")

(defvar aws-vpc-list-view-display-params
  '(name CidrBlock IsDefault)
  "List of attributes to display in list view.")

;; Model for Aws Vpc
(ecloud-define-resource-model aws vpc)

;; View for Aws Vpc
(ecloud-setup-resource-view aws vpc)

(defcustom aws-vpc-parser-hook
  '(aws-vpc--parse-address-space)
  "Hook to run for parsing address space."
  :group 'ecloud-aws
  :type 'hook)

;; Prepare data
(defun aws-vpc--prepare-data (data)
  "Function to prepare the `DATA for easier parsing."
  (-let (((&alist 'Vpcs vpclist) data))
    vpclist))

(defun aws-vpc--parse-name (data)
  "Function to parse the `DATA for name."
  (-let (((&alist 'Tags tags 'IsDefault default) data))
    (if (not (equal default :json-false)) "Default"
      (asoc-get (ecloud-map-tags-to-kv tags) "Name")))
  )

(defun aws-vpc--parse-address-space (robj)
  "Function to parse the address space for the vpc in the response `ROBJ."
  (-let* (((&alist 'addressSpace (&alist 'addressPrefixes address-list))
           (oref robj attributes))
          (address-list-str (string-join address-list ","))
          )
    (oset robj attributes (append (oref robj attributes)
                                  `((address-list . ,address-list-str))))
    ))

(provide 'aws-vpc)
;;; aws-vpc.el ends here
