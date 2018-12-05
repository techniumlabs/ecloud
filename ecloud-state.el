;;; ecloud-state.el --- Handle global state of ecloud app.  -*- lexical-binding: t -*-

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
;; Contains code to handle state management

;;; Code:

(require 'ht)
(require 's)
(require 'pcache)
(require 'asoc)
(require 'dash)
(require 'eieio)
(require 'ecloud-model)
(require 'f)
(eval-when-compile (require 'cl))

(defvar ecloud-state--current-state nil)
(defvar ecloud-state--errors (ht-create))
(defvar ecloud-default-no-fetch-time 600)

(cl-defun ecloud-state-init ()
  (setq ecloud-state--current-state (ht-create))

  (make-directory (format "%s/ecloud" pcache-directory) :parents)

  (--map
   (-let* ((reponame (format "%s" (string-remove-prefix (f-expand (format "%s" pcache-directory)) it)))
           (cloud (-last-item (-butlast (split-string reponame "/"))))
           (rtype (-last-item (split-string reponame "/"))))
     (pcache-map
      (pcache-repository reponame)
      (lambda (key value)
        (ecloud-state-update-resource cloud rtype key (oref value value)))))
   (directory-files-recursively (format "%s/ecloud/" pcache-directory)  "^[a-z]"))
  )

(cl-defun ecloud-state()
  "Get the global state."
  (unless ecloud-state--current-state
    (ecloud-state-init))
  ecloud-state--current-state)

(cl-defun ecloud-errors()
  "Get the global errors."
  ecloud-state--errors)

(cl-defun ecloud-register-cloud (cloud)
  "Register the CLOUD in the global state."
  (if (ht-get (ecloud-state) cloud nil)
      (ht-get (ecloud-state) cloud)
    (-let ((hash-table (ht-create)))
      (ht-set (ecloud-errors) cloud ())
      (ht-set (ecloud-state) cloud hash-table)
      hash-table
      )
    ))

(cl-defun ecloud-register-resource-type (cloud rtype)
  "Register a CLOUD resource of type RTYPE in the global state."
  (if (ht-get (ecloud-get-cloud-state cloud) rtype nil)
      (ht-get (ecloud-get-cloud-state cloud) rtype)
    (ht-set! (ecloud-get-cloud-state cloud) rtype
             (list (cons :data (ht-create)) (cons :metadata nil)))))

(cl-defun ecloud-get-cloud-state (cloud)
  "Return state for the CLOUD."
  (ht-get (ecloud-state) cloud (ecloud-register-cloud cloud)))

(cl-defun ecloud-get-resource-type-state (cloud rtype)
  "Return state for the resource type RTYPE in CLOUD."
  (ht-get (ecloud-get-cloud-state cloud) rtype (ecloud-register-resource-type cloud rtype)))

(cl-defun ecloud-set-resource-type-state (cloud rtype state)
  "Set the state for the resource type RTYPE in CLOUD."
  (ht-set! (ecloud-get-cloud-state cloud) rtype state))

(cl-defun ecloud-get-resource-type-data (cloud rtype)
  "Return data associated with state of resource type RTYPE for the CLOUD."
  (asoc-get (ecloud-get-resource-type-state cloud rtype) :data))

(cl-defun ecloud-get-resource-type-metadata (cloud rtype)
  "Return metadata associated with state of resource type RTYPE for the CLOUD."
  (asoc-get (ecloud-get-resource-type-state cloud rtype) :metadata))

(cl-defun ecloud-set-resource-type-metadata (cloud rtype metadata)
  "Update metadata for resource type RTYPE of CLOUD."
  (-let* ((data (ecloud-get-resource-type-data cloud rtype)))
    (ecloud-set-resource-type-state cloud rtype (list (cons :data data)
                                                      (cons :metadata metadata)))))

(cl-defun ecloud-get-resource-type-modified-ts (cloud rtype)
  "Return the last modified timestamp for RTYPE in CLOUD."
  (let ((lastupdated (asoc-get (ecloud-get-resource-type-metadata cloud rtype) :updatedts)))
    (if lastupdated lastupdated "0")))

(cl-defun ecloud-set-resource-type-modified-ts (cloud rtype ts)
  "Update the last modified timestamp for RTYPE in CLOUD."
  (-let ((metadata (ecloud-get-resource-type-metadata cloud rtype)))
    (asoc-pop! metadata :updatedts)
    (asoc-put! metadata :updatedts ts)
    (ecloud-set-resource-type-metadata cloud rtype metadata)
    ))

(cl-defun ecloud-state-clear-resources (cloud rtype)
  "Clear all the resources of a particular CLOUD and RTYPE from the global state."
  (ecloud-register-resource-type cloud rtype)
  (ht-set! (ht-get (ecloud-state) cloud) rtype (list (cons :data (ht-create)) (cons :metadata nil))))

(cl-defun ecloud-state-add-error (cloud summary error)
  "Add the latest ERROR and SUMMARY for the CLOUD to the top of the list."
  (ecloud-register-cloud cloud)
  (let ((err-list (ht-get (ecloud-errors) cloud)))
    (push (cons summary error) err-list)
    (ht-set! (ecloud-errors) cloud err-list))
  (ecloud-refresh-all-views))

(defun ecloud-state-get-errors (cloud)
  "Get the error list for the CLOUD."
  (ht-get (ecloud-errors) cloud))

(defun ecloud-state-resource-equalp (cloud rtype rname robj)
  "Check if there is an existing resource for CLOUD RTYPE with RNAME and ROBJ."
  (let ((rorig (ht-get (ecloud-get-resource-type-data cloud rtype) rname nil)))
    (if (and rorig robj
             (s-equals? (oref rorig name) (oref robj name))
             (s-equals? (oref rorig id) (oref robj id))
             (equal (ecloud-resource-has rorig) (ecloud-resource-has robj))
             (equal (ecloud-resource-belongs-to rorig) (ecloud-resource-belongs-to robj))
             (equal (oref rorig attributes) (oref robj attributes)))
        t
      nil)))

(defun ecloud-state-update-resource-type (cloud rtype objs &optional ts)
  "Update the resource type CLOUD RTYPE with list of OBJS and TS."
  (ecloud-state-clear-resources cloud rtype)
  (-each objs (lambda (it) (ecloud-state-update-resource cloud rtype (oref it name) it)))
  (ecloud-set-resource-type-modified-ts cloud rtype ts))

(defun ecloud-state-append-resource-type (cloud rtype objs &optional ts)
  "Add the resources to CLOUD RTYPE with list of OBJS and TS."
  (-each objs (lambda (it) (ecloud-state-update-resource cloud rtype (oref it name) it)))
  (ecloud-set-resource-type-modified-ts cloud rtype ts))

(defun ecloud-state-update-resource (cloud rtype rname robj)
  "Update the resource for CLOUD, RTYPE and RNAME with ROBJ and optional TS."
  (unless (ecloud-state-resource-equalp cloud rtype rname robj)
    (progn (ht-set! (ecloud-get-resource-type-data cloud rtype) (ecloud-resource-id robj) robj)
           ;; Cache the results for future use
           (let ((repo (pcache-repository (format "ecloud/%s/%s" cloud rtype))))
             (pcache-put repo rname robj))
           (ecloud-refresh-all-views))))

(defun ecloud-state--get-all-resource-type (cloud rtype &optional belongs-to)
  "Get all resource of a CLOUD RTYPE.
Optional `BELONGS-TO if present include only resources that belongs to the resource."
  (if belongs-to
      (ecloud-state--get-all-resource-belonging-to cloud rtype belongs-to)
    (--map (nth 1 it) (ht-items (cdr (assoc :data (ecloud-get-resource-type-state cloud rtype)))))))

(defun ecloud-state--get-resource-by-name (cloud rtype name)
  "Get resources of a CLOUD RTYPE with NAME."
  (--filter (equal (ecloud-resource-name it) name)
            (ecloud-state--get-all-resource-type cloud rtype)))

(defun ecloud-state--get-all-resource-belonging-to (cloud rtype robj)
  "Get all resource for `CLOUD `RTYPE that belongs to `ROBJ."
  (--filter (member (ecloud-resource-id robj) (ecloud-resource-belongs-to it (ecloud-resource-type robj)))
            (ecloud-state--get-all-resource-type cloud rtype))
  )

(cl-defun ecloud-parse-resource-data (data class &optional ts append)
  "Parse the resource data `DATA for the `CLASS and update metadata with timestamp `ts and return list of parsed data"
  (-let* (((cloud . rtype) (ecloud-class-to-cloud-and-rtype class))
          (nameAttr (intern (format "%s-%s--name-attribute" cloud rtype)))
          (nameAttrVal (if (boundp nameAttr) (symbol-value nameAttr) 'name))
          (parsed-data (--map (make-instance (if (stringp class) (intern class) class) :name (cdr (assoc nameAttrVal it))
                                             :id (if (cdr (assoc 'id it)) (cdr (assoc 'id it)) (cdr (assoc nameAttrVal it)))
                                             :attributes it
                                             :has nil
                                             :belongs-to nil) data)))

    (ecloud-register-resource-type cloud rtype)
    (if append
        (ecloud-state-append-resource-type cloud rtype parsed-data ts)
      (ecloud-state-update-resource-type cloud rtype parsed-data ts))
    (-each parsed-data (lambda (it) (run-hook-with-args (intern (format "%s-%s-parser-hook" cloud rtype)) it)))
    parsed-data
    ))

(cl-defun ecloud-fetch-resources (class &optional force)
  "Fetch updated resources for `CLASS on `FORCE or if no update time is expired"
  (-let* (((cloud . rtype) (ecloud-class-to-cloud-and-rtype class))
          (list-cmd-var-name (intern (format "%s--list-command" class)))
          (list-cmd (and (boundp list-cmd-var-name) (symbol-value list-cmd-var-name)))
          (global-params-var-name (intern (format "%s--global-params" class)))
          (global-params (and (boundp global-params-var-name) (symbol-value global-params-var-name)))
          (no-update-time-var-name (intern (format "%s--no-update-time" class)))
          (no-update-time (if (boundp no-update-time-var-name)(symbol-value no-update-time-var-name) ecloud-default-no-fetch-time))
          (ts (format-time-string "%s"))
          (lastts (ecloud-get-resource-type-modified-ts cloud rtype)))
    (if (or force (> (- (string-to-number ts) (string-to-number lastts)) no-update-time))
        (ecloud-run-json-command list-cmd
                                 global-params
                                 (lambda (json-output)
                                   (ecloud-parse-resource-data json-output class ts))))))

(cl-defun ecloud-fetch-resources-general (class cmd params &optional on-success on-failure)
  "Function to fetch resources given `CLASS. `CMD is the command to run to fetch the resources.
`PARAMS is the additional params that you want to pass.
`ON-SUCCESS is the callback function to run on success. If none provided default parser is used.
`ON-FAILURE is the callback function to run on failure. If none provided no action is taken."
  (-let* ((ts (format-time-string "%s")))
    (if on-success
        (ecloud-run-json-command cmd params on-success on-failure)
      (ecloud-run-json-command cmd params (lambda (json-output)
                                            (ecloud-parse-resource-data json-output class ts))))))

(provide 'ecloud-state)
;;; ecloud-state.el ends here
