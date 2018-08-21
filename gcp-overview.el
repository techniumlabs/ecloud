;;; gcp-overview.el --- Overview Buffer for GCP -*- lexical-binding: t -*-

;; Author: Ramanathan Sivagurunathan
;; Maintainer: Ramanathan Sivagurunathan
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (magit "2.13.0"))
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'magit-popup)
(require 'ecloud-vars)
(require 'ecloud-view)
(require 'gcp-project)

(defcustom gcp-overview-list-views
  '(project)
  "Components that are part of the gcp overview view"
  :package-version '(ecloud . "0.0.1")
  :group 'ecloud)

(magit-define-popup gcp-overview-dispatch-popup
  "Popup console for showing an overview of available popup commands."
  :group 'ecloud
  :actions
  '("Popup and dwim commands"
    (?a "Account" gcp-account-popup)))

(defcustom gcp-overview-sections-hook
  '(gcp-insert-overview-headers
    gcp-insert-views
    gcp-insert-error-view)
  "Hook run to insert sections into a status buffer."
  :package-version '(ecloud . "0.0.1")
  :group 'ecloud
  :type 'hook)

(defun gcp-insert-overview-headers ()
  "Insert header sections appropriate for `magit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`magit-status-headers-hook'."
  (insert "Gcp Cloud\n\n"))

(defun gcp-insert-views ()
  "Insert the defined views"
  (ecloud-insert-list-views 'gcp gcp-overview-list-views))

(defun gcp-insert-error-view ()
  "Insert the error view"
  (ecloud-insert-error-view 'gcp))

(defun gcp-overview-refresh-buffer ()
  (interactive)
  ;; Trigger Refresh data
  (--map (let ((rtype (intern (format "gcp-%s" it))))
           (ecloud-fetch-resources rtype))
        gcp-overview-list-views)
  (gcp-overview-refresh-view))

(defun gcp-overview-refresh-view ()
  (magit-insert-section (status)
    (magit-run-section-hook 'gcp-overview-sections-hook)))

(defvar gcp-overview-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;;TODO
    (define-key keymap (kbd "h") 'gcp-overview-dispatch-popup)
    (define-key keymap (kbd "r") 'gcp-overview-refresh-buffer)
    keymap)
  "Keymap for `gcp-overview-mode'.")

;;;###autoload
;; TODO Update
(define-derived-mode gcp-overview-mode ecloud-mode "Gcp Overview"
  "Mode for working with gcp overview."
  :group 'ecloud)

;;;###autoload
(defun gcp-overview ()
  "Display an overview buffer for gcp"
  (interactive)
  (gcp-overview-internal default-directory))

(defun gcp-overview-internal (directory)
  (ecloud-mode-setup #'gcp-overview-mode))

(provide 'gcp-overview)
;;; gcp-overview.el ends here
