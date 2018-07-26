;;; gcp-commands.el --- Handle gcp generic commands.  -*- lexical-binding: t; -*-

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

(defun gcp-commands-get-info (cb &optional cleanup-cb)
  "Get all pods and execute callback CB with the parsed JSON.
PROPS is an alist of functions to inject.  It should normally be passed
`kubernetes-props'.
STATE is the application state.
CLEANUP-CB is a function taking no arguments used to release any resources."
  (ecloud-run-json-command "gcloud" '("info" "--format" "json")
                      (lambda (json-output)
                          (funcall cb json-output))
                      cleanup-cb))

(provide 'gcp-commands)
;;; gcp-commands.el ends here
