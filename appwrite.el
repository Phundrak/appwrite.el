;;; appwrite.el --- Appwrite server SDK  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Lucien Cartier-Tilet

;; Author: Lucien Cartier-Tilet <lucien@phundrak.com>
;; Maintainer: Lucien Cartier-Tilet <lucien@phundrak.com>
;; URL: https://github.com/Phundrak/appwrite.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: extensions, lisp, database, appwrite, tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Appwrite server SDK for Emacs
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

(defgroup appwrite nil
  "Customizationi group for `appwrite'."
  :group 'emacs-lisp
  :link '(url-link :tag "Gitea" "https://labs.phundrak.com/phundrak/appwrite.el")
  :link '(url-link :tag "Github" "https://github.com/Phundrak/appwrite.el"))

(defcustom appwrite-endpoint ""
  "Appwrite endpoint.
Must not include the API version, e.g.
\"https://appwrite.example.org\".  The variable must not end with
a trailing forward slash.  Setting this variable with
`customize-set-variable' takes care of it automatically."
  :group 'appwrite
  :type 'string
  :set (lambda (symbol val)
         (set-default symbol
                      (if (string-suffix-p "/" val)
                          (message "%s" (substring val 0 (1- (length val))))
                        val))))

(defvar appwrite-api-key ""
  "API Key for accessing your Appwrite API.
Be sure to keep it safe and never upload its value somewhere on
the internet, even if it’s a private repository.")

(defvar appwrite-project ""
  "ID of the project to act on.")


;;; Internal functions

(defun appwrite--plistp (object)
  "Non-nil if and only if OBJECT is a valid plist.
Compatibility function for Emacs 27 and earlier, the code source
in the else branch is the definition of `plistp' in Emacs 29."
  (if (boundp #'plistp)
      (plistp object)
    (let ((len (proper-list-p object)))
      (and len (zerop (% len 2))))))

(defun appwrite--get-full-url (api)
  "Get the full url for API.
If API does not begin with an initial forward slash, add it
automatically.  If it does not contain an API version, prefix
\"/v1\" by default."
  (let ((versionp (string-match-p "^/?v[[:digit:]]+.*" api))
        (initial-forward-slash-p (string-prefix-p "/" api)))
    (concat appwrite-endpoint
            (if versionp
                ""
              "/v1")
            (if initial-forward-slash-p
                ""
              "/")
            api)))

(defun appwrite--message-failure (message status json-message)
  "Display MESSAGE followed by JSON-MESSAGE.
This will show a message in the modeline in this format:

    [status STATUS] MESSAGE: JSON-MESSAGE"
  (message "[status %d] %s: %s" status message json-message))

(defun appwrite--process-response (message success-status response)
  "In case of failure when calling the Appwrite API, display MESSAGE.
The function considers a call to the API a failure in case the
HTTP status code in RESPONSE differs from SUCCESS-STATUS, the
HTTP status code hoped for.  If that’s the case, warn the user,
see `appwrite--message-failure', else return the JSON returned by
the API."
  (let ((status (car response))
        (json (cdr response)))
    (if (= status success-status)
        json
      (appwrite--message-failure message status (gethash "message" json)))))

(cl-defun appwrite--query-api (&key (method "GET")
                                    api
                                    payload
                                    (content-type "application/json")
                                    payload-alist-p
                                    asyncp
                                    callback)
  "Perform a method METHOD to API with PAYLOAD as its payload.
CONTENT-TYPE is whichever miime-type is being used.

If CONTENT-TYPE is \"application/json\", PAYLOAD is subject ot
automatic conversion depending on its type.
- If PAYLOAD passes `appwrite--plistp', it will be converted to
  JSON as a plist.
- If PAYLOAD passes `hash-table-p', it will be converted to JSON
  as a hash table.
- If PAYLOAD-ALIST-P is t, PAYLOAD will be converted to JSON as an
  associative table.
- Else, PAYLOAD will be passed a string containing JSON.

If ASYNCP is t, `appwrite--post-api' will be asynchronous.
CALLBACK must then be set as it will be called once the request
finishes.  See `url-retrieve'.

The function returns a pair composed of the HTTP status code as
its car.  The cdr is a hash table from the response answer if
Content-Type in the headers is \"application/json\"."
  (let* ((url (appwrite--get-full-url api))
         (url-request-method method)
         (url-request-extra-headers `(("X-Appwrite-key"     . ,appwrite-api-key)
                                      ("X-Appwrite-Project" . ,appwrite-project)
                                      ("Content-type"       . ,content-type)))
         (url-request-data (cond ((not (string= content-type "application/json"))
                                  payload)
                                 ((appwrite--plistp payload)
                                  (json-encode-plist payload))
                                 ((hash-table-p payload) (json-encode payload))
                                 (payload-alist-p        (json-encode-alist payload))
                                 (t                   payload))))
    (if asyncp
        (url-retrieve url callback)
      (with-current-buffer (url-retrieve-synchronously url)
        (let (http-code json)
          (message ";;;;;;;;;;;;")
          (message "%s" (buffer-string))
          (save-match-data
            (goto-char (point-min))
            (re-search-forward (rx bol "HTTP" (+ (not space)) " " (group (+ digit))))
            (setq http-code (string-to-number
                             (buffer-substring-no-properties (match-beginning 1)
                                                             (match-end 1)))))
          (when (re-search-forward "^Content-Type: application/json" nil t)
            (goto-char (point-min))
            (re-search-forward "^$")
            (delete-region (point) (point-min))
            (setq json (json-parse-buffer)))
          `(,http-code . ,json))))))


;;; Account


;;; Users


;;; Teams


;;; Databases


;;; Storage

(cl-defun appwrite--storage-update-bucket (id
                                           name
                                           &key
                                           updatep
                                           (permission "bucket")
                                           read write (enabled t)
                                           maximum-file-size allowed-file-extensions
                                           (encryption t) (antivirus t))
  "Create or update a storage bucket.
Create or update a storage bucket named NAME with id ID.

If UPDATEP is t, update the bucket, else create it.

PERMISSION is the permissioin type model to use for reading files
in this bucket.  By default, PERMISSION is \"bucket\". For more
info, see https://appwrite.io/docs/permissions

READ is an array of roles for read permissions.

WRITE is an array of roles for write permissions.

If ENABLED is nil, disable bucket, t by default.

MAXIMUM-FILE-SIZE is an integer indicating in bytes the maximum
size of an uploaded. The default is 30MB (not MiB), though this
might be different on self-hosted instances.

ALLOWED-FILE-EXTENSIONS is an array of allowed file extensions. A
maximum of 100 extensions no longer than 64 characters are
allowed.

If ENCRYPTION is t, enable encryption for the bucket. Files
larger than 20MB are skipped. t by default.

If ANTIVIRUS is t, enable antivirus for the bucket. Files larger
than 20MB are skipped. t by default."
  (let ((payload `(bucketId ,id name ,name permission ,permission))
        (method (if updatep "PUT" "POST")))
    (when read
      (setq payload (append payload `(read ,read))))
    (when write
      (setq payload (append payload `(write ,write))))
    (when maximum-file-size
      (setq payload (append payload `(maximumFileSize ,maximum-file-size))))
    (when allowed-file-extensions
      (setq payload (append payload `(allowedFileExtensions ,allowed-file-extensions))))
    (setq payload (append payload `(enabled ,(if enabled t :json-false))))
    (setq payload (append payload `(encryption ,(if encryption t :json-false))))
    (setq payload (append payload `(antivirus ,(if antivirus t :json-false))))
    ;; (json-encode-plist payload)
    (let ((response (appwrite--query-api :method method
                                         :api (concat "/v1/storage/buckets/"
                                                      (if updatep id ""))
                                         :payload (json-encode-plist payload))))
      (appwrite--process-response (format "Failed to %s bucket %s"
                                          (if updatep "update" "create")
                                          id)
                                  (if updatep 200 201)
                                  response))))

(cl-defun appwrite-storage-create-bucket (id
                                          name
                                          &key
                                          (permission "bucket")
                                          read write (enabled t)
                                          maximum-file-size allowed-file-extensions
                                          (encryption t) (antivirus t))
  "Create storage bucket.
For documentation on ID, NAME, PERMISSION, READ, WRITE, ENABLED,
MAXIMUM-FILE-SIZE, ALLOWED-FILE-EXTENSIONS, ENCRYPTION, and
ANTIVIRUS, check `appwrite--storage-update-bucket'."
  (appwrite--storage-update-bucket id
                                   name
                                   :updatep nil
                                   :permission permission
                                   :read read
                                   :write write
                                   :enabled enabled
                                   :maximum-file-size maximum-file-size
                                   :allowed-file-extensions allowed-file-extensions
                                   :encryption encryption
                                   :antivirus antivirus))

(cl-defun appwrite-storage-update-bucket (id
                                          name
                                          &key
                                          (permission "bucket")
                                          read write (enabled t)
                                          maximum-file-size allowed-file-extensions
                                          (encryption t) (antivirus t))
  "Create storage bucket.
For documentation on ID, NAME, PERMISSION, READ, WRITE, ENABLED,
MAXIMUM-FILE-SIZE, ALLOWED-FILE-EXTENSIONS, ENCRYPTION, and
ANTIVIRUS, check `appwrite--storage-update-bucket'."
  (appwrite--storage-update-bucket id
                                   name
                                   :updatep t
                                   :permission permission
                                   :read read
                                   :write write
                                   :enabled enabled
                                   :maximum-file-size maximum-file-size
                                   :allowed-file-extensions allowed-file-extensions
                                   :encryption encryption
                                   :antivirus antivirus))

(cl-defun appwrite-storage-list-buckets (&key search (limit 25) offset cursor cursor-direction order-type)
  "List of all storage buckets.

SEARCH is a string to filter the list results when non-nil.  Max
length of 256 chars.

LIMIT is the maximum amount of buckets returned by the
query (default value: 25).

OFFSET is the results offset with which the user can manage the
pagination of the results when non-nil.

CURSOR is the id of the bucket used as the starting point of the
query, excluding the bucket itself.

CURSOR-DIRECTION can be either \\='after or \\='before.

ORDER-TYPE can be either \\='ascending or \\='descending.

If the query is successful, return a hash table made from the
acquired JSON.  Otherwise, return nil and warn the user."
  (let (payload)
    (when search (setq payload (append payload `(search ,search))))
    (setq payload (append payload `(limit ,limit)))
    (when offset (setq payload (append payload `(offset ,offset))))
    (when cursor (setq payload (append payload `(cursor ,cursor))))
    (when cursor-direction (setq payload (append payload `(cursor-direction ,cursor-direction))))
    (when order-type (setq payload (append payload `(order-type ,order-type))))
    (let* ((response (appwrite--query-api :api "/v1/storage/buckets"
                                          :payload (json-encode-plist payload)))
           (status (car response))
           (json (cdr response)))
      (if (eq 200 status)
          json
        (appwrite--message-failure "Failed to list buckets" 200 (gethash "message" json))))))

(defun appwrite-storage-get-bucket (id)
  "Get bucket with id ID."
  (let ((response (appwrite--query-api :api (concat "/v1/storage/buckets/" id))))
    (appwrite--process-response (concat "Failed to get bucket " id)
                                200
                                response)))

(defun appwrite-storage-delete-bucket (id)
  "Delete bucket with id ID."
  (let ((response (appwrite--query-api :method "DELETE"
                                       :api (concat "/v1/storage/buckets/" id))))
    (appwrite--process-response (concat "Failed to delete bucket " id)
                                204
                                response)))


;;; Functions


;;; Localization


;;; Avatars


;;; Health

(provide 'appwrite)
;;; appwrite.el ends here
