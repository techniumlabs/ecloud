;;; ecloud-view.el --- Handle VIEW operations for resources.  -*- lexical-binding: t; -*-

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
(eval-when-compile (require 'cl))

(defun ecloud-insert-list-views (cloud views)
  (--map (-let* ((view-name (format "%s-%s" cloud it))
                 (params-name (intern (format "%s-%s-list-view-display-params" cloud it)))
                 (robjs (ecloud-state--get-all-resource-type
                         (symbol-name cloud)
                         (symbol-name it)))
                 (align-length (-flatten
                                (--map
                                 (-max (-flatten
                                        (->> robjs
                                             (-map
                                              (-lambda (obj)
                                                (length (ecloud-get-attributes (nth 1 obj) it))))
                                             )))
                                 (symbol-value params-name))))
                 (flist (->> align-length
                             (--map (format "%%-%ds  " it))
                             (-reduce 'concat)
                             )))

           (magit-insert-section (view-name)
             (magit-insert-heading (format "%s %s" cloud it))
             (magit-insert-section (view-name)
               (insert (propertize (apply #'format flist (symbol-value params-name))
                                   'face 'magit-section-heading))
               (insert ?\n))
             (-map (-lambda ((name obj))
                     (-let ((strout (apply #'format flist (-map (lambda (x) (ecloud-get-attributes obj x))(symbol-value params-name)))))
                       (eval `(magit-insert-section (,view-name ,obj)
                                (insert ,strout)
                                (insert ?\n)))))
                   robjs)
             (insert ?\n)
             )) views))

(provide 'ecloud-view)
;;; ecloud-view.el ends here
