;;; ecloud-commands.el --- Handle external commands.  -*- lexical-binding: t; -*-

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

(defun ecloud-process-kill-quietly (proc &optional _signal)
  (when proc
    (set-process-sentinel proc nil)
    (set-process-query-on-exit-flag proc nil)
    (let ((kill-buffer-query-functions nil)
          (buf (process-buffer proc)))
      (ignore-errors (kill-process proc))
      (ignore-errors (delete-process proc))
      (ignore-errors (kill-buffer buf)))))

(defun ecloud-run-command (cmd args on-success &optional on-error cleanup-cb)
  "Run a command with ARGS.
PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.
STATE is the current application state, used to apply additional
global flags to kubectl.
ON-SUCCESS is a function of one argument, called with the process' buffer.
Optional ON-ERROR is a function of two arguments, called with the
process' stderr buffer.  If omitted, it defaults to
`kubernetes-kubectl--default-error-handler', which logs an error
if the process exited unexpectedly.
Optional CLEANUP-CB is a function of no arguments that is always
called after the other callbacks.  It can be used for releasing
resources.
After callbacks are executed, the process and its buffer will be killed.
Returns the process object for this execution of kubectl."
  (let* ((buf (generate-new-buffer " ecloud"))
         (err-buf (generate-new-buffer " ecloud-err"))
         (command (append cmd args ))

         ;; `default-directory' must exist, otherwise `make-process' raises an
         ;; error.
         ;; (default-directory (kubernetes-utils-up-to-existing-dir default-directory))
         (proc (make-process
                :name "ecloud"
                :buffer buf
                :stderr err-buf
                :command command
                :noquery t
                :sentinel
                (lambda (proc status)
                  (unwind-protect
                      (let ((exit-code (process-exit-status proc)))
                        (cond
                         ;; Success Handler
                         ((zerop exit-code)
                          (funcall on-success buf)
                          )
                         ;; Failure Handler
                         (t
                          (let ((err-message (with-current-buffer err-buf (buffer-string))))
                            (unless (= 9 exit-code)
                              ;; (kubernetes-props-update-last-error props err-message (string-join command " ") (current-time))
                              (funcall on-success buf)
                              ))
                          (cond (on-error
                                 (funcall on-error err-buf))
                                (t
                                 ;; TODO Add default error handler
                                 (message err-message)
                                 ;; (kubernetes-kubectl--default-error-handler props status)
                                 )))
                         ))
                    (when cleanup-cb
                      (funcall cleanup-cb))
                    (ecloud-process-kill-quietly proc)))
                )))

    ;; Clean up stderr buffer when stdout buffer is killed.
    (with-current-buffer buf
      (add-hook 'kill-buffer-hook (lambda ()
                                    (let ((kill-buffer-query-functions nil))
                                      (ignore-errors (kill-buffer err-buf))))
                nil t))

    proc))

(defun ecloud-parse-json-buffer (buf)
  (let ((json (with-current-buffer buf (json-read-from-string (buffer-string)))))
    json))

(defun ecloud-run-json-command (cmd args on-success &optional on-error cleanup-cb)
  (ecloud-run-command cmd args
                      (lambda (buf)
                        (funcall on-success (ecloud-parse-json-buffer buf)))
                      on-error
                      cleanup-cb))


(provide 'ecloud-commands)
;;; ecloud-commands.el ends here
