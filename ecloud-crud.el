;;; ecloud-crud.el --- Handle CRUD operations for resources.  -*- lexical-binding: t; -*-

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
(require 'ht)
(eval-when-compile (require 'cl))

(require 'ecloud-commands)

(defclass ecloud-base-resource ()
  ((type :initarg :type)
   (attributes :type list))
  "Container for holding all resources of a type"
  )

(cl-defmethod ecloud-get-attributes ((robj ecloud-base-resource) attrib-name)
  (if (slot-exists-p robj (intern (if (symbolp attrib-name) (symbol-name attrib-name) attrib-name )))
      (eval `(oref ,robj ,(intern (if (symbolp attrib-name) (symbol-name attrib-name) attrib-name ))))
    (cdr (assoc attrib-name (oref robj attributes)))))

(cl-defmacro ecloud-define-resource-model (cloud name &rest body)
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

(cl-defmacro ecloud-define-simple-resource-action (name command &rest body)
  (cl-assert (symbolp name))
  `(cl-defun ,name ()
     (interactive)
     (let* ((section (magit-current-section))
            (type (oref section type))
            (value (oref section value)))

       (ecloud-run-json-command (--map (if (stringp it)
                                           it
                                         (ecloud-get-attributes value it))
                                       ',command)
                                nil
                                (lambda (json-output) (message "%s" json-output))))))

(cl-defmacro ecloud-define-cautious-action (name command prompt &rest body)
  (cl-assert (symbolp name))
  `(cl-defun ,name ()
     (interactive)
     (let* ((section (magit-current-section))
            (type (oref section type))
            (value (oref section value)))

       (if ',prompt
           (-if-let* ((confirm (magit-confirm t (apply 'format (--map (if (stringp it)
                                                                          it
                                                                        (ecloud-get-attributes value it))
                                                                      ',prompt)))))
               (ecloud-run-json-command (--map (if (stringp it)
                                                   it
                                                 (ecloud-get-attributes value it))
                                               ',command)
                                        nil
                                        (lambda (json-output) (message "%s" json-output)))
             )))))

(cl-defun ecloud-parse-resource-data (data class)

  (-let* ((cloud (nth 0 (split-string (format "%s" class) "-")))
         (rtype (nth 1 (split-string (format "%s" class) "-")))
         (nameAttr (intern (format "%s-%s--name-attribute" cloud rtype)))
         (nameAttr (if (boundp nameAttr) (symbol-value nameAttr) :name ))
         (parsed-data (--map (make-instance class :name (cdr (assoc 'name it))
                                            :id (cdr (assoc 'id it))
                                            :attributes it) data)))
    (ecloud-register-resource cloud rtype)
    (--map (progn
             (run-hook-with-args (intern (format "%s-%s-parser-hook" cloud rtype)) it)
             (ecloud-state-update cloud rtype (oref it :name) it))
           parsed-data)))

(cl-defun ecloud-fetch-resources (class)
  (let* ((list-cmd (intern (format "%s--list-command" class)))
         (list-cmd (and (boundp list-cmd) (symbol-value list-cmd)))
         (global-params (intern (format "%s--global-params" class)))
         (global-params (and (boundp global-params) (symbol-value global-params))))

    (ecloud-run-json-command list-cmd
                             global-params
                             (lambda (json-output)
                               (message "Json Output: %s" json-output)
                               (ecloud-parse-resource-data json-output class)))
    ))

(provide 'ecloud-crud)
;;; ecloud-crud.el ends here
