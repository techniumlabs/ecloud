;;; ecloud-state.el --- Handle global state of ecloud app.  -*- lexical-binding: t; -*-

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
(require 'pcache)
(eval-when-compile (require 'cl))

(defvar ecloud-state--current-state (ht-create))
(defvar ecloud-state--errors (ht-create))

(defun ecloud-state()
  "Get the global state"
  ecloud-state--current-state)

(defun ecloud-errors()
  "Get the global errors"
  ecloud-state--errors)

(defun ecloud-register-cloud (cloud)
  "Register the cloud in the global state"
  (unless (ht-get (ecloud-state) cloud)
    (progn
      (ht-set (ecloud-state) cloud (ht-create))
      (ht-set (ecloud-errors) cloud ()))))

(defun ecloud-register-resource (cloud rtype)
  "Register a cloud resource in the global state"
  (ecloud-register-cloud cloud)
  (unless (ht-get (ht-get (ecloud-state) cloud) rtype)
    (ht-set (ht-get (ecloud-state) cloud) rtype (ht-create))))

(defun ecloud-state-clear-resources (cloud rtype)
  "Clear all the resources of a particular type from the global state"
  (ecloud-register-resource cloud rtype)
  (ht-set! (ht-get (ecloud-state) cloud) rtype (ht-create)))

(defun ecloud-state-add-error (cloud error)
  "Add the latest error to the top of the list"
  (ecloud-register-cloud cloud)
  (let ((err-list (ht-get (ecloud-errors) cloud)))
    (push error err-list)
    (message "Error list is %s" err-list)
    (ht-set! (ecloud-errors) cloud err-list))
  (ecloud-refresh-all-views))

(defun ecloud-state-get-errors (cloud)
  "Get the error list for the cloud"
  (ht-get (ecloud-errors) cloud))

(defun ecloud-state-update (cloud rtype rname robj &optional args)
  (ecloud-register-resource cloud rtype)
  (ht-set! (ht-get (ht-get (ecloud-state) cloud) rtype) rname robj)
  ;; Cache the results for future use
  (let ((repo (pcache-repository cloud)))
    (pcache-put repo rtype (ht-get (ht-get (ecloud-state) cloud) rtype)))
  (ecloud-refresh-all-views))

(defun ecloud-state--get-all-resource-type (cloud rtype)
  (ht-items (ht-get (ht-get (ecloud-state) cloud) rtype)))

(cl-defun ecloud-define-resource-action (cloud rtype action &rest body)
  (cl-assert (symbolp cloud))
  (cl-assert (symbolp rtype))

  (let ((fname (intern (format "%s-%s--%s" cloud rtype action)))
        (default-action-fn (intern (format "ecloud-resource-default-action--%s" action))))
    `(cl-defun ,fname (data)
       (apply ',default-action-fn '(',cloud ',rtype data)))))

(defmacro ecloud-define-resource-state (cloud rname &optional actions &rest body)
  (cl-assert (symbolp cloud))
  (cl-assert (symbolp rname))
  (cl-assert (listp actions))

  `(progn ,@(mapcar (lambda (x) (ecloud-define-resource-action 'azure 'account x)) actions))

  )

(provide 'ecloud-state)
;;; ecloud-state.el ends here
