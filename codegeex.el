;;; codegeex.el --- CodeGeeX For Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Hao Zhang

;; Author: Hao Zhang <hzhangxyz@outlook.com>
;; Keywords: codegeex, completion
;; Version: 0.0.1

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

;; This package provide the completion drived by CodeGeeX API, which relies on
;; reverse engineering on vscode extension.
;; CodeGeeX official website: https://codegeex.cn/
;; VSCode extension: https://marketplace.visualstudio.com/items?itemName=aminer.codegeex

;;; Code:

(require 'url)
(require 'json)
(require 'uuidgen)

(defvar codegeex-apikey "68cf004321e94b47a91c2e45a8109852" "API key obtained from CodeGeeX website")
(defvar codegeex-apisecret "e82b86a16f9d471ab215f653060310e3" "API secret obtained from CodeGeeX website")
(defvar codegeex-temperature 0.2 "temperature for completion by CodeGeeX")
(defvar codegeex-top_p 0.95 "top_p for completion by CodeGeeX")
(defvar codegeex-top_k 0 "top_k for completion by CodeGeeX")
(defvar codegeex-extinfo `((sid . ,(uuidgen-4))
                           (ide . "Emacs")
                           (ideVersion . ,emacs-version)) "The ext field in JSON to be sent to server")

(defun codegeex-completion-invoke (prefix suffix lang)
  "Invoke CodeGeeX completion API.

This function will complete code between PREFIX and SUFFIX, which are usually
the content before cursor and after cursor, and put the result to the current
buffer. LANG is the programming lanuauge of the code."
  (let* ((url "https://tianqi.aminer.cn/api/v2/multilingual_code_generate_adapt")
         (data (json-encode `((prompt . ,prefix)
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
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data data))
    (url-retrieve
     url
     (lambda (status parent-buffer)
       (goto-char (point-min))
       (re-search-forward "^$")
       (delete-region (point) (point-min))
       (let* ((json-string (buffer-string))
              (json-data (json-read-from-string json-string))
              (json-result (assoc-default 'result json-data))
              (json-output (assoc-default 'output json-result))
              (json-code (assoc-default 'code json-output))
              (result (aref json-code 0)))
         (with-current-buffer parent-buffer
           (insert result))
         (kill-buffer)))
     `(,(current-buffer))))
  nil)

(defun codegeex-debug-invoke (prompt lang begin end)
  "Invoke the codegeex debugger API.

PROMPT is the code content to be debugged. LANG is the programming language name
of the code. The result will be replaced between BEGIN and END in the current
buffer."
  (let* ((url "https://tianqi.aminer.cn/api/v2/multilingual_code_bugfix")
         (data (json-encode `((prompt . ,prompt)
                              (n . 1)
                              (apikey . ,codegeex-apikey)
                              (apisecret . ,codegeex-apisecret)
                              (lang . ,lang)
                              (ext . ,codegeex-extinfo))))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data data))
    (url-retrieve
     url
     (lambda (status parent-buffer begin end)
       (goto-char (point-min))
       (re-search-forward "^$")
       (delete-region (point) (point-min))
       (let* ((json-string (buffer-string))
              (json-data (json-read-from-string json-string))
              (json-result (assoc-default 'result json-data))
              (json-output (assoc-default 'output json-result))
              (json-code (assoc-default 'code json-output))
              (result (aref json-code 0)))
         (with-current-buffer parent-buffer
           (delete-region begin end)
           (insert result))
         (kill-buffer)))
     `(,(current-buffer) ,begin ,end)))
  nil)

(defun codegeex-language ()
  "Return the language of the current buffer."
  (interactive)
  (let* ((name (symbol-name major-mode))
         (name-without-mode (replace-regexp-in-string "-mode" "" name)))
    name-without-mode))

;;;###autoload
(defun codegeex-buffer-completion ()
  "CodeGeeX buffer completion.

The completion result will be put in the position of cursor directly."
  (interactive)
  (message "CodeGeeX completing")
  (let ((prefix (buffer-substring (point-min) (point)))
        (suffix (buffer-substring (point) (point-max))))
    (codegeex-completion-invoke prefix suffix (codegeex-language)))
  nil)

;;;###autoload
(defun codegeex-buffer-debug ()
  "CodeGeeX debugging for the current buffer.

The result will be replaced into the buffer directly."
  (interactive)
  (message "CodeGeeX debugging")
  (let* ((begin (point-min))
         (end (point-max))
         (prompt (buffer-substring begin end)))
    (codegeex-debug-invoke prompt (codegeex-language) begin end))
  nil)

;;;###autoload
(defun codegeex-region-debug ()
  "CodeGeeX debugging of region.

The result will be replaced into the selected region."
  (interactive)
  (message "CodeGeeX debugging")
  (let* ((begin (region-beginning))
         (end (region-end))
         (prompt (buffer-substring begin end)))
    (codegeex-debug-invoke prompt (codegeex-language) begin end))
  nil)

;;;###autoload
(define-key global-map (kbd "M-\\") 'codegeex-buffer-completion)

(provide 'codegeex)
;;; codegeex.el ends here
