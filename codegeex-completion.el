;;; codegeex-completion.el --- CodeGeeX For Emacs      -*- lexical-binding: t; -*-

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

;; Heavily inspired by copilot.el

;;; Code:

(require 'codegeex-api)
(require 'codegeex-overlay)

(defun codegeex-completion--show-completion (completion)
  (save-excursion
    (save-restriction
      (widen)
      (let* ((p (point))
             (start p)
             (end (+ p (length completion))))
        (codegeex--display-overlay-completion
         completion start end)))))

(defun codegeex-completion--get-completion (callback)
  "Retrieve context (prefix and suffix) and language and invoke `codegeex-api--get-completion'
CALLBACK is launched with json result of the call"
  (let ((prefix (buffer-substring (point-min) (point)))
        (suffix (buffer-substring (point) (point-max)))
        (language (codegeex-language)))
    (codegeex-api--get-completion
     prefix suffix language callback)))

(provide 'codegeex-completion)
;;; codegeex-completion.el ends here
