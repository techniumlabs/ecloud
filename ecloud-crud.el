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


(defclass ecloud-base-resource ()
  ((type :initarg :type)
   (attributes :type list))
  "Container for holding all resources of a type"
  )

(cl-defmethod ecloud-get-attributes ((robj ecloud-base-resource) attrib-name)
  (if (slot-exists-p robj attrib-name)
      (eval `(oref ,robj ,attrib-name))
    (cdr (assoc attrib-name (oref robj attributes)))))

(defmacro ecloud-define-resource-model (cloud name &rest body)
  (cl-assert (symbolp cloud))
  (cl-assert (symbolp name))
  (let ((classname (intern (format "%s-%s" cloud name))))
    `(progn
       (defclass ,classname (ecloud-base-resource)
         ((type :initform ,name)
          (id :initarg :id)
          (name :initarg :name)
          (attributes :initarg :attributes)))
       )))

(cl-defun ecloud-parse-resource-data (data class)
  (-let ((parsed-data (--map (make-instance class :name (cdr (assoc 'name it))
                                            :id (cdr (assoc 'id it))
                                            :attributes it) data)))

    (-let ((cloud (nth 0 (split-string (format "%s" class) "-")))
           (rtype (nth 1 (split-string (format "%s" class) "-"))))
      (ecloud-state-clear-resources cloud rtype))

    (--map (-let ((cloud (nth 0 (split-string (format "%s" class) "-")))
                  (rtype (nth 1 (split-string (format "%s" class) "-")))
                  (rname (oref it :name)))
             (run-hook-with-args (intern (format "%s-%s-parser-hook" cloud rtype)) it)
             (ecloud-state-update cloud rtype rname it)
             ) parsed-data)
    )
  )

(cl-defun ecloud-fetch-resources (class)
  (message "Fetching all")
  (let* ((list-cmd (intern (format "%s--list-command" class)))
         (list-cmd (and (boundp list-cmd) (symbol-value list-cmd)))
         (global-params (intern (format "%s--global-params" class)))
         (global-params (and (boundp global-params) (symbol-value global-params))))

    (ecloud-run-json-command list-cmd
                             global-params
                             (lambda (json-output)
                               (message "%s" json-output)
                               (ecloud-parse-resource-data json-output class)))
    ))

(provide 'ecloud-crud)
;;; ecloud-crud.el ends here
