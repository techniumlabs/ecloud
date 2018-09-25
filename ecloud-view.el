;;; ecloud-view.el --- Handle VIEW operations for resources.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 The Ecloud Contributors

;; Author: Ramanathan Sivagurunathan <ramzthecoder+ecloud@gmail.com>

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (magit "2.13.0") (ht "2.2") (s "1.12.0") (pcache "0.4.2"))

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

(defun ecloud-refresh-all-views ()
  (dolist (buffer (ecloud-mode-get-buffers))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall (intern (format "%s-refresh-view" (substring (symbol-name major-mode) 0 -5))))))))

(defun ecloud-insert-list-views (cloud views)
  (--map (-let* ((view-name (format "%s-%s" cloud it))
                 (params-name (intern (format "%s-%s-list-view-display-params" cloud it)))
                 (robjs (ecloud-state--get-all-resource-type
                         (symbol-name cloud)
                         (symbol-name it)))
                 (align-length (if (> (length robjs) 0)
                                   (-flatten
                                    (--map
                                     (-max (-flatten
                                            (list (->> robjs
                                                  (-map
                                                   (-lambda (obj)
                                                     (length (let ((aval (ecloud-get-attributes (nth 1 obj) it)))
                                                               (cond ((equal :json-false aval) "false")
                                                                     ((equal t aval) "true")
                                                                     ((stringp aval) aval))))))
                                                  ) (length (symbol-name it)))))
                                     (symbol-value params-name)))
                                 0
                                 ))
                 (flist (if (> (length robjs) 0 )
                            (->> align-length
                              (--map (+ 3 it))
                              (--map (format "%%-%ds  " it))
                              (-reduce 'concat)
                              )
                          "%s")))

           (magit-insert-section (view-name)
             (magit-insert-heading (format "%s %s" cloud it))
             (magit-insert-section (view-name)
               (if (> (length robjs) 0)
                   (insert (propertize (apply #'format flist (symbol-value params-name))
                                       'face 'magit-section-heading))
                 (insert (propertize (format "No %s found" it)
                                     'face 'magit-section-heading)))
               (insert ?\n))
             (if (> (length robjs) 0)
                 (-map (-lambda ((name obj))
                         (-let ((strout (apply #'format flist (-map (lambda (x) (let ((aval (ecloud-get-attributes obj x)))
                                                                                  (cond ((equal :json-false aval) "false")
                                                                                        ((equal t aval) "true")
                                                                                        ((stringp aval) aval))))
                                                                    (symbol-value params-name)))))
                           (eval `(magit-insert-section (,view-name ,obj)
                                    (insert ,strout)
                                    (insert ?\n)))))
                       robjs))
             (insert ?\n)
             )) views))

(defun ecloud-insert-error-view (cloud)
  (let ((errors (ecloud-state-get-errors cloud)))
    (magit-insert-section (errors)
      (if errors
          (progn
            (magit-insert-heading "Errors")
            (-map (lambda (e)
                    (progn (magit-insert-section (errors e)
                            (magit-insert-heading (car e))
                            (insert (cdr e))))) errors))
        (magit-insert-heading "No Errors")
        )
      )))

(defmacro ecloud-setup-resource-view (cloud rtype)
  `(progn
     (define-derived-mode ,(intern (format "%s-%s-overview-mode" cloud rtype)) ecloud-mode
       ,(format "%s %s resource overview" cloud rtype)
       ,(format "Mode for working with %s %s overview mode" cloud rtype)
       :group 'ecloud)

     (defvar ,(intern (format "%s-%s-overview-mode-map" cloud rtype))
       nil
       (format "Keymap for %s-%s-overview-mode" ,cloud ,rtype))

     (defun ,(intern (format "%s-%s-overview" cloud rtype))()
       (interactive)
       (ecloud-mode-setup #',(intern (format "%s-%s-overview-mode" cloud rtype))))

     (defun ,(intern (format "%s-%s-overview-refresh-view" cloud rtype)) ()
       (magit-insert-section (,(intern (format "%s-%s" cloud rtype))))
       (ecloud-insert-list-views ',cloud '(,rtype)))

     (defun ,(intern (format "%s-%s-overview-refresh-buffer" cloud rtype)) ()
       (interactive)
       (magit-insert-section (,(intern (format "%s-%s" cloud rtype))))
       (ecloud-insert-list-views ',cloud '(,rtype)))))

(provide 'ecloud-view)
;;; ecloud-view.el ends here
