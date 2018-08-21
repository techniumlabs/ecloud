;;; ecloud-mode.el --- ecloud mode -*- lexical-binding: t -*-

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


;;; Commentary:

;; TODO Add commentary

;;; Code:
(defvar ecloud-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map   [tab]     'magit-section-toggle)
    map))

(define-derived-mode ecloud-mode special-mode "ecloud"
  "Base mode for ecloud.

\\{ecloud-mode-map}"

  :group 'ecloud
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'magit-section-update-highlight t t)
  (add-hook 'deactivate-mark-hook #'magit-section-update-highlight t t)
  (setq-local redisplay-highlight-region-function 'magit-highlight-region)
  (setq-local redisplay-unhighlight-region-function 'magit-unhighlight-region)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1))

  ;; TODO
  ;; (add-hook 'kill-buffer-hook 'magit-preserve-section-visibility-cache)

  )

(defun ecloud-mode-get-buffers ()
  (--filter (with-current-buffer it
              (and (derived-mode-p 'ecloud-mode)))
            (buffer-list)))

(defun ecloud-mode-get-buffer (mode &optional create frame value)
  (or (--first (with-current-buffer it
                 (and (eq major-mode mode)
                      (if value
                          (and magit-buffer-locked-p
                               (equal (magit-buffer-lock-value) value))
                        (not magit-buffer-locked-p))))
               (if frame
                   (mapcar #'window-buffer
                           (window-list (unless (eq frame t) frame)))
                 (buffer-list)))
      (and create
           (magit-generate-new-buffer mode value))))

(defun ecloud-mode-setup (mode &rest args)
  "Setup up a MODE buffer using ARGS to generate its content."
  (ecloud-mode-setup-internal mode args))

(defun ecloud-mode-setup-internal (mode args &optional locked)
  "Setup up a MODE buffer using ARGS to generate its content.
When optional LOCKED is non-nil, then create a buffer that is
locked to its value, which is derived from MODE and ARGS."
  (let ((buffer (ecloud-mode-get-buffer
                 mode t nil
                 (and locked (magit-buffer-lock-value mode args))))
        (section (magit-current-section)))
    (with-current-buffer buffer
      (setq magit-previous-section section)
      (setq magit-refresh-args args)
      (funcall mode))
    (magit-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'magit-mode-setup-hook)
      (magit-refresh-buffer))))

(provide 'ecloud-mode)

;;; ecloud-mode.el ends here
