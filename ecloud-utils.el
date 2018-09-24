;;; ecloud-utils.el --- Util functions for ecloud -*- lexical-binding: t -*-

;; Author: Ramanathan Sivagurunathan
;; Maintainer: Ramanathan Sivagurunathan
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (magit "2.13.0") (ht "2.2") (s "1.12.0") (pcache "0.4.2"))
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

(defun ecloud-display-buffer-fullframe (buffer)
  (let ((display-fn
         (lambda (buffer alist)
           (when-let (window (or (display-buffer-reuse-window buffer alist)
                                 (display-buffer-same-window buffer alist)
                                 (display-buffer-pop-up-window buffer alist)
                                 (display-buffer-use-some-window buffer alist)))
             (delete-other-windows window)
             window))))
    (display-buffer buffer (list display-fn)))
  )

(defun ecloud-display-buffer (buffer)
  (let ((window (funcall ecloud-display-buffer-function buffer)))
    (when ecloud-display-buffer-select
      (select-frame-set-input-focus
       (window-frame (select-window window)))))
  )

(defun magit-read-int (prompt &optional initial-input history default-value
                                 inherit-input-method no-whitespace)
  "Read an integer from the minibuffer, prompting with string PROMPT.

* empty input is not allowed
* whitespace is not allowed and leading and trailing whitespace is
  removed automatically,
* \": \" is appended to PROMPT, and
* an invalid DEFAULT-VALUE is silently ignored."
  (when default-value
    (when (consp default-value)
      (setq default-value (car default-value)))
    (unless (integerp default-value)
      (setq default-value 1)))
  (let* ((minibuffer-completion-table nil)
         (val (read-from-minibuffer
               (magit-prompt-with-default (concat prompt ": ") default-value)
               initial-input (and no-whitespace magit-minibuffer-local-ns-map)
               nil history default-value inherit-input-method))
         (trim (lambda (regexp string)
                 (save-match-data
                   (if (string-match regexp string)
                       (replace-match "" t t string)
                     string)))))
    (setq val (funcall trim "\\`\\(?:[ \t\n\r]+\\)"
                       (funcall trim "\\(?:[ \t\n\r]+\\)\\'" val)))
    (cond ((string= val "")
           (user-error "Need non-empty input"))
          ((not (integerp (string-to-number val)))
           (user-error "Need an integer input. %s is not integer" val))
          (t (string-to-number val)))))

(provide 'ecloud-utils)

;;; ecloud-utils.el ends here
