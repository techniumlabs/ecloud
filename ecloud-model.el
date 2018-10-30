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

(cl-defmethod ecloud-resource-name ((robj ecloud-base-resource))
  "Returns name of the resource instance `ROBJ"
  (oref robj name))

(cl-defmethod ecloud-resource-id ((robj ecloud-base-resource))
  "Returns id of the resource instance `ROBJ"
  (oref robj id))

(cl-defmethod ecloud-resource-attribute ((robj ecloud-base-resource) attrib-name)
  "Get attribute `ATTRIB-NAME for `ROBJ"
  (if (slot-exists-p robj (intern (if (symbolp attrib-name) (symbol-name attrib-name) attrib-name )))
      (eval `(oref ,robj ,(intern (if (symbolp attrib-name) (symbol-name attrib-name) attrib-name ))))
    (cdr (assoc attrib-name (oref robj attributes)))))

(cl-defmethod ecloud-resource-belongs-to-type ((robj ecloud-base-resource))
  "Returns associated resource type that `ROBJ belongs to"
  (asoc-keys (oref robj belongs-to)))

(cl-defmethod ecloud-resource-belongs-to ((robj ecloud-base-resource) &optional type)
  "Returns associated resource that `ROBJ belongs to"
  (asoc-get (oref robj belongs-to) type))

(cl-defmethod ecloud-resource-add-belongs-to ((robj ecloud-base-resource) type value)
  "Add a new belongs to association for `ROBJ with `TYPE and `VALUE"
  (-let ((res (ecloud-resource-belongs-to robj type))
         (btolist (oref robj belongs-to)))
    (push value res)
    (asoc-put! btolist type res t)
    (oset robj belongs-to btolist)))

(cl-defmethod ecloud-resource-delete-belongs-to ((robj ecloud-base-resource) type value)
  "Deletes an existing belongs to association for `ROBJ with `TYPE and `VALUE"
  (-let* ((res (ecloud-resource-belongs-to robj type))
          (newres (delete value res))
          (btolist (oref robj belongs-to)))
    (asoc-put! btolist type newres t)
    (oset robj belongs-to btolist)))

(cl-defmethod ecloud-resource-has-type ((robj ecloud-base-resource))
  "Returns associated resource type that `ROBJ has"
  (asoc-keys (oref robj has)))

(cl-defmethod ecloud-resource-has ((robj ecloud-base-resource) &optional type)
  "Returns for `ROBJ the associated resource of `TYPE"
  (asoc-get (oref robj has) type))

(cl-defmethod ecloud-resource-add-has ((robj ecloud-base-resource) type value)
  "Add a new association for `ROBJ to `TYPE and `VALUE"
  (let ((res (ecloud-resource-has robj type))
        (haslist (oref robj has)))
    (push value res)
    (asoc-put! haslist type res t)
    (oset robj has haslist)))

(cl-defmethod ecloud-resource-delete-has ((robj ecloud-base-resource) type value)
  "Remove an existing association for `ROBJ to `TYPE and `VALUE"
  (let* ((res (ecloud-resource-has robj type))
         (newres (delete value res))
         (haslist (oref robj has)))
    (asoc-put! haslist type newres t)
    (oset robj has haslist)))

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
          (attributes :initarg :attributes)
          (belongs-to :initarg :belongs-to)
          (has :initarg :has)))
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
                                         (ecloud-resource-attribute value it))
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
                                                                        (ecloud-resource-attribute value it))
                                                                      ',prompt)))))
               (ecloud-run-json-command (--map (if (stringp it)
                                                   it
                                                 (ecloud-resource-attribute value it))
                                               ',command)
                                        nil
                                        (lambda (json-output) (message "%s" json-output)))
             )))))

(provide 'ecloud-model)
;;; ecloud-model.el ends here
