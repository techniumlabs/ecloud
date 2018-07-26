;;; ecloud-crud.el --- Handle CRUD operations for resources.  -*- lexical-binding: t; -*-

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
(require 'ht)
(eval-when-compile (require 'cl))

(defclass ecloud-resource-list ()
  ((type :initarg :type)
   (command :type list)
   (resources :type list)
   )
  "Container for holding all resources of a type"
  )

(cl-defmethod parse ((obj ecloud-resource-list) data)
  (message "Should be implemented by the subclass")
  )

(cl-defmethod fetch ((obj ecloud-resource-list))
  (ecloud-run-json-command (car (oref obj command))
                           (cdr (oref obj command))
                           (lambda (json-output)
                             (parse obj json-output)))
  )

(defclass ecloud-resource ()
  ((name :initarg :name)
   (type :initarg :type)
   (attributes :initarg :attributes))
  "ecloud resource"
  )

(cl-defmethod update-attribute ((obj ecloud-resource) key value)
  ;; (ht-set! (oref obj attributes) key value)
  (message "To be implemented")
  )

(cl-defmethod init-attributes ((obj ecloud-resource) value)
  (oset obj attributes value)
  )

(cl-defmethod create-resource ((obj ecloud-resource))
  (message "Create is not yet implemented"))

(cl-defmethod read-resource ((obj ecloud-resource))
  (message "Read is not yet implemented"))

(cl-defmethod update-resource ((obj ecloud-resource))
  (message "Upate is not yet implemented"))

(cl-defmethod delete-resource ((obj ecloud-resource))
  (message "Delete is not yet implemented"))


(provide 'ecloud-crud)
;;; ecloud-crud.el ends here
