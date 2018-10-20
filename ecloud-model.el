;;; ecloud-model.el --- Handle MODEL operations for resources.  -*- lexical-binding: t; -*-

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
;; Contains code to handle model operations for resources

;;; Code:

(require 'eieio)
(require 'ht)
(require 'subr-x)
(eval-when-compile (require 'cl))

(require 'ecloud-commands)

(defclass ecloud-base-resource ()
  ((type :initarg :type)
   (attributes :type list))
  "Container for holding all resources of a type."
  )

(cl-defmethod ecloud-get-attributes ((robj ecloud-base-resource) attrib-name)
  "Get attribute `ATTRIB-NAME for `ROBJ"
  (if (slot-exists-p robj (intern (if (symbolp attrib-name) (symbol-name attrib-name) attrib-name )))
      (eval `(oref ,robj ,(intern (if (symbolp attrib-name) (symbol-name attrib-name) attrib-name ))))
    (cdr (assoc attrib-name (oref robj attributes)))))

(cl-defmacro ecloud-define-resource-model (cloud name)
  "Create a resource model for `CLOUD and resource `NAME"
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

(cl-defmacro ecloud-define-simple-resource-action (name command)
  "Define a simple action `NAME and run the `COMMAND on invocation"
  (cl-assert (symbolp name))
  `(cl-defun ,name ()
     (interactive)
     (let* ((section (magit-current-section))
            (value (oref section value)))

       (ecloud-run-json-command (--map (if (stringp it)
                                           it
                                         (ecloud-get-attributes value it))
                                       ',command)
                                nil
                                (lambda (json-output) (message "%s" json-output))))))

(cl-defmacro ecloud-define-cautious-action (name command prompt)
  "Define a cautious action `NAME and run the `COMMAND on invocation but ask for `PROMPT for confirmation"
  (cl-assert (symbolp name))
  `(cl-defun ,name ()
     (interactive)
     (let* ((section (magit-current-section))
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

(provide 'ecloud-model)
;;; ecloud-model.el ends here
