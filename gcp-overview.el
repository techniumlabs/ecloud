;;; gcp-overview.el --- Overview Buffer for GCP -*- lexical-binding: t -*-

;; Author: Ramanathan Sivagurunathan
;; Maintainer: Ramanathan Sivagurunathan
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (magit "2.8.0"))
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


;;; Commentary:

;; TODO Add commentary

;;; Code:

(require 'ecloud-vars)
(require 'gcp-vars)


(defcustom gcp-overview-sections-hook
  '(gcp-insert-overview-headers)
  "Hook run to insert sections into a status buffer."
  :package-version '(ecloud . "0.0.1")
  :group 'ecloud
  :type 'hook)

(defun gcp-insert-overview-headers ()
  "Insert header sections appropriate for `magit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`magit-status-headers-hook'."
  (insert "In the beginning there was private cloud\n\n")
  (magit-insert-section (info)
    (gcp-commands-get-info (lambda (output) (insert output)))
    )
  )

(defun gcp-overview-refresh-buffer ()
  (magit-insert-section (status)
    (magit-run-section-hook 'gcp-overview-sections-hook)
    )
  )

(defun gcp-overview--initialize-buffer ()
  "Setup the gcp overview buffer for the first time"
  (let ((buf (get-buffer-create gcp-overview-buffer-name)))
    (with-current-buffer buf
      (gcp-overview-mode)
      ;; TODO
      ;;(add-hook 'kubernetes-redraw-hook #'kubernetes-overview--redraw-buffer)
      ;;(add-hook 'kubernetes-poll-hook #'kubernetes-overview--poll)
      ;;(kubernetes-timers-initialize-timers)
      ;;(kubernetes-overview--redraw-buffer)
      ;;(add-hook 'kill-buffer-hook (kubernetes-utils-make-cleanup-fn buf) nil t)
      ) buf)
  )

(defvar gcp-overview-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;;TODO
    (define-key keymap (kbd "v") 'gcp-overview-set-sections)
    keymap)
  "Keymap for `gcp-overview-mode'.")

;;;###autoload
;; TODO Update
(define-derived-mode gcp-overview-mode ecloud-mode "GCP Overview"
  "Mode for working with gcp overview.
\\<kubernetes-overview-mode-map>\
Type \\[kubernetes-overview-set-sections] to choose which resources to display.
Type \\[kubernetes-mark-for-delete] to mark an object for deletion, and \\[kubernetes-execute-marks] to execute.
Type \\[kubernetes-unmark] to unmark the object at point, or \\[kubernetes-unmark-all] to unmark all objects.
Type \\[kubernetes-navigate] to inspect the object on the current line.
Type \\[kubernetes-copy-thing-at-point] to copy the thing at point.
Type \\[kubernetes-refresh] to refresh the buffer.
\\{kubernetes-overview-mode-map}"
  :group 'ecloud)

;;;###autoload
(defun gcp-overview ()
  "Display an overview buffer for gcp"
  (interactive)
  (gcp-overview-internal default-directory)
  )

(defun gcp-overview-internal (directory)
  (magit--tramp-asserts directory)
  (let ((default-directory directory))
    (magit-mode-setup #'gcp-overview-mode)))

(provide 'gcp-overview)

;;; gcp-overview.el ends here
