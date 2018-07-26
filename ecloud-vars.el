;;; ecloud-vars.el --- Customizable interface for ecloud.  -*- lexical-binding: t; -*-

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

(defcustom ecloud-display-buffer-select t
  "Whether to select ecloud buffers automatically."
  :group 'ecloud
  :type 'boolean)

(defcustom ecloud-display-buffer-function 'display-buffer
  "The function used display a ecloud buffer.

The function must take a single argument, which is the buffer to display."
  :group 'ecloud
  :type '(radio (function-item ecloud-display-buffer-fullframe)
                (function-item display-buffer)
                (function :tag "Function")))


(provide 'ecloud-vars)

;;; ecloud-vars.el ends here
