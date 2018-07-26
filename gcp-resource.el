;;; gcp-resource.el --- GCP Resource definition.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 The Ecloud Contributors

;; Author: Ramanathan Sivagurunathan <ramzthecoder+ecloud@gmail.com>

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (magit "2.8.0"))

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

;;; project
(defclass gcp-project-list (ecloud-resource-list)
  ((type :initform "project")
   (command :initform ("gcloud" "projects" "list" "--format" "json")))
  "List of gcp projects"
  )

(cl-defmethod parse ((obj gcp-project-list) data)
  (oset obj resources (--map (-let (((&alist 'createTime create-time
                                             'lifecycleState lifecycle-state
                                             'name name
                                             'projectId project-id
                                             'projectNumber project-number) it))
                               (gcp-project :name name :attributes (list (cons :create-time create-time)
                                                                         (cons :name name)
                                                                         (cons :project-id project-id)
                                                                         (cons :project-number project-number)
                                                                         (cons :lifecycle-state lifecycle-state)
                                                                         )))
                             data))
  )

(defclass gcp-project (ecloud-resource)
  ((type :initform "project"))
  )

(provide 'gcp-resource)
;;; gcp-resource.el ends here
