;;; codegeex-api.el --- CodeGeeX For Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Samuel D

;; Author: Samuel D <samueld@mailo.com>
;; Keywords: codegeex, completion

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is handling the api of codegeex

;;; Code:

;; COMPLETION

(defun codegeex-api--get-completion (prefix suffix lang callback)
  "Invoke CodeGeeX completion API.

This function will complete code between PREFIX and SUFFIX, which are usually
the content before cursor and after cursor, and put the result to the current
buffer. LANG is the programming lanuauge of the code
CALLBACK is launched with the content of the buffer."
  (let ((url (concat codegeex-endpoint "multilingual_code_generate_adapt"))
        (data (codegeex-api--generate-json-data prefix suffix lang)))
    (codegeex-api--url-retrieve url data callback)))

(defun codegeex-api--url-retrieve (url json-data callback)
  "Default url retrieve as POST with json data

TODO : CALLBACK takes a json plist as argument"
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data json-data))
    (url-retrieve
     url
     (lambda (status init-buffer callback)
       (let ((result (codegeex-api--get-json-result)))
         (with-current-buffer init-buffer
           (funcall callback result))))
     `(,(current-buffer) ,callback) t)))

(defun codegeex-api--get-json-result ()
  "Get the code string from the json response
TODO: Create a plist with the json response to avoid that
kind of shit"
  (goto-char (point-min))
  (re-search-forward "^$")
  (aref
   (assoc-default
    'code
    (assoc-default
     'output
     (assoc-default
      'result
      (json-read-from-string
       (buffer-substring (point) (point-max))))))
   0))

(defun codegeex-api--generate-json-data (prefix suffix lang)
  "Create Json-encoded data to send to codegeex API"
  (json-encode `((prompt . ,prefix)
                 (suffix . ,suffix)
                 (n . 1)
                 (apikey . ,codegeex-apikey)
                 (apisecret . ,codegeex-apisecret)
                 (temperature . ,codegeex-temperature)
                 (top_p . ,codegeex-top_p)
                 (top_k . ,codegeex-top_k)
                 (isFimEnabled . ,(not (equal suffix "")))
                 (lang . ,lang)
                 (ext . ,codegeex-extinfo))))

(provide 'codegeex-api)
;;; codegeex-api.el ends here
