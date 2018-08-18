;;; azure-resource.el --- AZURE Resource definition.  -*- lexical-binding: t; -*-

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

(require 'eieio)
(eval-when-compile (require 'cl))

;;; Account
(defclass azure-account-list (ecloud-resource-list)
  ((type :initform "account")
   (command :initform ("az" "account" "list" "--output" "json")))
  "List of azure Accounts"
  )

(cl-defmethod parse ((obj azure-account-list) data)
  (oset obj resources (--map (-let (((&alist 'id id
                                             'state state
                                             'name name
                                             'tenantId tenant-id
                                             'idDefault default
                                             'user (&alist :name user-name :type user-type)) it))
                               (azure-account :name name :attributes (list (cons :id id)
                                                                         (cons :name name)
                                                                         (cons :state state)
                                                                         (cons :default default)
                                                                         (cons :tenant-id tenant-id)
                                                                         (cons :user-type user-type)
                                                                         (cons :user-name user-name)
                                                                         )))
                             data))
  )

(defclass azure-account(ecloud-resource)
  ((type :initform "account"))
  )

;;; Resource Group
(defclass azure-resource-group-list (ecloud-resource-list)
  ((type :initform "group")
   (command :initform ("az" "group" "list" "--output" "json")))
  "List of azure Group"
  )

(cl-defmethod parse ((obj azure-resource-group-list) data)
  (oset obj resources (--map (-let (((&alist 'id id
                                             'location location
                                             'name name
                                             'properties (&alist :provisioningState provisioning-state)) it))
                               (azure-resource-group :name name :attributes (list (cons :id id)
                                                                           (cons :location location)
                                                                           (cons :name name)
                                                                           (cons :provisioning-state provisioning-state)
                                                                         )))
                             data))
  )

(defclass azure-resource-group(ecloud-resource)
  ((type :initform "resource-group"))
  )

;;; vm

(provide 'azure-resource)
;;; azure-resource.el ends here
