;;; ecloud-state.el --- Handle global state of ecloud app.  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'eieio)
(require 'ht)
(require 'pcache)
(require 'asoc)
(eval-when-compile (require 'cl))

(defvar ecloud-state--current-state nil)
(defvar ecloud-state--errors (ht-create))

(defun ecloud-state-init ()
  (setq ecloud-state--current-state (ht-create))
  (--map
   (-let* ((reponame (format "%s" (string-remove-prefix (format "%s" pcache-directory) it)))
           (cloud (nth 1 (split-string reponame "/")))
           (rtype (nth 2 (split-string reponame "/"))))
     (pcache-map
      (pcache-repository reponame)
      (lambda (key value)
        (ecloud-state-update-resource cloud rtype key (oref value :value)))))
   (directory-files-recursively (format "%s/ecloud/" pcache-directory)  "^[a-z]"))
  )

(defun ecloud-state()
  "Get the global state"
  (unless ecloud-state--current-state
    (ecloud-state-init))
  ecloud-state--current-state)

(defun ecloud-errors()
  "Get the global errors"
  ecloud-state--errors)

(defun ecloud-register-cloud (cloud)
  "Register the CLOUD in the global state"
  (if (ht-get (ecloud-state) cloud nil)
      (ht-get (ecloud-state) cloud)
    (-let ((hash-table (ht-create)))
      (ht-set (ecloud-errors) cloud ())
      (ht-set (ecloud-state) cloud hash-table)
      hash-table
      )
    ))

(defun ecloud-register-resource-type (cloud rtype)
  "Register a CLOUD resource of type RTYPE in the global state"
  (if (ht-get (ecloud-get-cloud-state cloud) rtype nil)
      (ht-get (ecloud-get-cloud-state cloud) rtype)
    (ht-set! (ecloud-get-cloud-state cloud) rtype
             (list (cons :data (ht-create)) (cons :metadata nil)))))

(defun ecloud-get-cloud-state (cloud)
  "Return state for the CLOUD"
  (ht-get (ecloud-state) cloud (ecloud-register-cloud cloud)))

(defun ecloud-get-resource-type-state (cloud rtype)
  "Return state for the resource type RTYPE in CLOUD"
  (ht-get (ecloud-get-cloud-state cloud) rtype (ecloud-register-resource-type cloud rtype)))

(defun ecloud-set-resource-type-state (cloud rtype state)
  "Set the state for the resource type RTYPE in CLOUD"
  (ht-set! (ecloud-get-cloud-state cloud) rtype state))

(defun ecloud-get-resource-type-data (cloud rtype)
  "Return data associated with state of resource type RTYPE for the CLOUD"
  (asoc-get (ecloud-get-resource-type-state cloud rtype) :data))

(defun ecloud-get-resource-type-metadata (cloud rtype)
  "Return metadata associated with state of resource type RTYPE for the CLOUD"
  (cdr (asoc-get (ecloud-get-resource-type-state cloud rtype) :metadata)))

(defun ecloud-set-resource-type-metadata (cloud rtype metadata)
  "Update metadata for resource type RTYPE of CLOUD"
  (-let* ((data (ecloud-get-resource-type-data cloud rtype)))
    (ecloud-set-resource-type-state cloud rtype (list (cons :data data)
                                                      (cons :metadata metadata)))))

(defun ecloud-get-resource-type-modified-ts (cloud rtype)
  "Return the last modified timestamp for RTYPE in CLOUD"
  (asoc-get (ecloud-get-resource-type-metadata cloud rtype) :updatedts))

(defun ecloud-set-resource-type-modified-ts (cloud rtype ts)
  "Update the last modified timestamp for RTYPE in CLOUD"
  (-let ((metadata (ecloud-get-resource-type-metadata cloud rtype)))
    (asoc-pop! metadata :updatedts)
    (asoc-put! metadata :updatedts ts)
    (ecloud-set-resource-type-metadata cloud rtype metadata)
    ))

(defun ecloud-state-clear-resources (cloud rtype)
  "Clear all the resources of a particular type from the global state"
  (ecloud-register-resource-type cloud rtype)
  (ht-set! (ht-get (ecloud-state) cloud) rtype (list (cons :data (ht-create)) (cons :metadata nil))))

(defun ecloud-state-add-error (cloud summary error)
  "Add the latest error to the top of the list"
  (ecloud-register-cloud cloud)
  (let ((err-list (ht-get (ecloud-errors) cloud)))
    (push (cons summary error) err-list)
    (ht-set! (ecloud-errors) cloud err-list))
  (ecloud-refresh-all-views))

(defun ecloud-state-get-errors (cloud)
  "Get the error list for the cloud"
  (ht-get (ecloud-errors) cloud))

(defun ecloud-state-resource-equalp (cloud rtype rname robj)
  (-if-let (rorig (ht-get (ecloud-get-resource-type-data cloud rtype) rname nil))
    (if (and (string-equal (oref rorig :name) (oref robj :name))
             (string-equal (oref rorig :id) (oref robj :id))
             (equal (oref rorig :attributes) (oref robj :attributes)))
        t
      nil)))

(defun ecloud-state-update-resource-type (cloud rtype objs &optional ts)
  (ecloud-state-clear-resources cloud rtype)
  (--map (ecloud-state-update-resource cloud rtype (oref it :name) it)
         objs)
  (ecloud-set-resource-type-modified-ts cloud rtype (format-time-string "%s")))

(defun ecloud-state-update-resource (cloud rtype rname robj &optional ts)
  (unless (ecloud-state-resource-equalp cloud rtype rname robj)
    (progn (ht-set! (ecloud-get-resource-type-data cloud rtype) rname robj)

     ;; Cache the results for future use
     (let ((repo (pcache-repository (format "ecloud/%s/%s" cloud rtype))))
       (pcache-put repo rname robj))
     (ecloud-refresh-all-views))))

(defun ecloud-state--get-all-resource-type (cloud rtype)
  (ht-items (cdr (assoc :data (ecloud-get-resource-type-state cloud rtype)))))

(provide 'ecloud-state)
;;; ecloud-state.el ends here
