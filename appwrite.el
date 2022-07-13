;;; appwrite.el --- Appwrite server SDK for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Lucien Cartier-Tilet

;; Author: Lucien Cartier-Tilet <lucien@phundrak.com>
;; Maintainer: Lucien Cartier-Tilet <lucien@phundrak.com>
;; URL: https://github.com/Phundrak/appwrite.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: database emacs-lisp

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
\"https://appwrite.example.org\". The variable must not end with
a trailing forward slash. Setting this variable with
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

(defun appwrite--get-full-url (api)
  "Get the full url for API.
If API does not begin with an initial forward slash, add it
automatically.

If it does not contain an API version, prefix \"/v1\" by default."
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

(cl-defun appwrite--query-api (&key (method "GET")
                                    api
                                    data
                                    (content-type "application/json")
                                    data-alist-p
                                    asyncp
                                    callback)
  "Perform a method METHOD to API with DATA as its payload.
CONTENT-TYPE is whichever miime-type is being used.

If CONTENT-TYPE is \"application/json\", DATA is subject ot
automatic conversion depending on its type.
- If DATA passes `plistp', it will be converted to JSON as a
  plist.
- If DATA passes `hash-table-p', it will be converted to JSON as
  a hash table.
- If DATA-ALIST-P is t, DATA will be converted to JSON as an
  associative table.
- Else, DATA will be passed a string containing JSON.

If ASYNCP is t, `appwrite--post-api' will be asynchronous.
CALLBACK must then be set as it will be called once the request
finishes. See `url-retrieve'."
  (let* ((url (appwrite--get-full-url api))
         (url-request-method method)
         (url-request-extra-headers `(("X-Appwrite-key"     . ,appwrite-api-key)
                                      ("X-Appwrite-Project" . ,appwrite-project)
                                      ("Content-type"       . ,content-type)))
         (url-request-data (cond ((not (string= content-type "application/json"))
                                  data)
                                 ((plistp data)       (json-encode-plist data))
                                 ((hash-table-p data) (json-encode data))
                                 (data-alist-p        (json-encode-alist data))
                                 (t                   data))))
    (if asyncp
        (url-retrieve url callback)
      (with-current-buffer (url-retrieve-synchronously url)
        (buffer-string)))))


;;; Account


;;; Users


;;; Teams


;;; Databases


;;; Storage

(cl-defun appwrite-storage-create-bucket (id
                                          name
                                          &key
                                          (permission "bucket")
                                          read write (enabled t)
                                          maximum-file-size allowed-file-extensions
                                          (encryption t) (antivirus t))
  "Create bucket.
Create bucket named NAME with id ID.

PERMISSION is the permissioin type model to use for reading files
in this bucket. By default, PERMISSION is \"bucket\". For more
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
  (let ((data `(bucketId ,id name ,name permission ,permission)))
    (when read
      (setq data (append data `(read ,read))))
    (when write
      (setq data (append data `(write ,write))))
    (when maximum-file-size
      (setq data (append data `(maximumFileSize ,maximum-file-size))))
    (when allowed-file-extensions
      (setq data (append data `(allowedFileExtensions ,allowed-file-extensions))))
    (setq data (append data `(enabled ,(if enabled t :json-false))))
    (setq data (append data `(encryption ,(if encryption t :json-false))))
    (setq data (append data `(antivirus ,(if antivirus t :json-false))))
    ;; (json-encode-plist data)
    (appwrite--query-api :method "POST" :api "storage/buckets" :data (json-encode-plist data))))

(defun appwrite-storage-delete-bucket (id)
  "Delete bucket with id ID."
  (appwrite--query-api :method "DELETE"
                       :api (concat "storage/buckets/" id)))


;;; Functions


;;; Localization


;;; Avatars


;;; Health

(provide 'appwrite)
;;; appwrite.el ends here
