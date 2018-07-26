;;; azure-overview.el --- Overview Buffer for AZURE -*- lexical-binding: t -*-

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
(require 'azure-vars)


(defcustom azure-overview-sections-hook
  '(azure-insert-overview-headers)
  "Hook run to insert sections into a status buffer."
  :package-version '(ecloud . "0.0.1")
  :group 'ecloud
  :type 'hook)

(defun azure-insert-overview-headers ()
  "Insert header sections appropriate for `magit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`magit-status-headers-hook'."
  (insert "In the beginning there was private cloud\n\n")
  (magit-insert-section (info)
    (azure-commands-get-info (lambda (output) (insert output)))
    )
  )

(defun azure-overview-refresh-buffer ()
  (magit-insert-section (status)
    (magit-run-section-hook 'azure-overview-sections-hook)
    )
  )

(defun azure-overview--initialize-buffer ()
  "Setup the azure overview buffer for the first time"
  (let ((buf (get-buffer-create azure-overview-buffer-name)))
    (with-current-buffer buf
      (azure-overview-mode)
      ;; TODO
      ;;(add-hook 'kubernetes-redraw-hook #'kubernetes-overview--redraw-buffer)
      ;;(add-hook 'kubernetes-poll-hook #'kubernetes-overview--poll)
      ;;(kubernetes-timers-initialize-timers)
      ;;(kubernetes-overview--redraw-buffer)
      ;;(add-hook 'kill-buffer-hook (kubernetes-utils-make-cleanup-fn buf) nil t)
      ) buf)
  )

(defvar azure-overview-mode-map
  (let ((keymap (make-sparse-keymap)))
    ;;TODO
    (define-key keymap (kbd "v") 'azure-overview-set-sections)
    keymap)
  "Keymap for `azure-overview-mode'.")

;;;###autoload
;; TODO Update
(define-derived-mode azure-overview-mode ecloud-mode "Azure Overview"
  "Mode for working with azure overview.
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
(defun azure-overview ()
  "Display an overview buffer for azure"
  (interactive)
  (azure-overview-internal default-directory)
  )

(defun azure-overview-internal (directory)
  (magit--tramp-asserts directory)
  (let ((default-directory directory))
    (magit-mode-setup #'azure-overview-mode)))

(provide 'azure-overview)

;;; azure-overview.el ends here
