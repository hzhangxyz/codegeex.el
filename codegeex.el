;;; codegeex.el --- CodeGeeX For Emacs      -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Hao Zhang

;; Author: Hao Zhang <hzhangxyz@outlook.com>
;; Contributor: Samuel Dawant <samueld@mailo.com>
;; Keywords: codegeex, completion
;; Version: 0.1.0

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
(require 'codegeex-completion)
(require 'codegeex-api)
(require 'codegeex-overlay)


(defvar codegeex-json-string-cache nil
  "Mainly for debugging but maybe will be useful")

(defvar codegeex-request-cache nil
  "Mainly for debugging but maybe will be useful")

(defvar codegeex-response-cache nil
  "Mainly for debugging but maybe will be useful")

(defcustom codegeex-idle-delay 0.5
  "Time in seconds to wait before starting completion. Complete immediately if set to 0."
  :type 'float
  :group 'codegeex)

(defcustom codegeex-clear-overlay-ignore-commands nil
  "List of commands that should not clear the overlay when called."
  :group 'codegeex
  :type '(repeat function))

(defvar codegeex-endpoint "https://tianqi.aminer.cn/api/v2/"
  "the endpoint of CodeGeeX API")

(defvar codegeex-apikey "68cf004321e94b47a91c2e45a8109852"
  "API key obtained from CodeGeeX website")

(defvar codegeex-apisecret "e82b86a16f9d471ab215f653060310e3"
  "API secret obtained from CodeGeeX website")

(defvar codegeex-temperature 0.2
  "temperature for completion by CodeGeeX")

(defvar codegeex-top_p 0.95
  "top_p for completion by CodeGeeX")

(defvar codegeex-top_k 0
  "top_k for completion by CodeGeeX")

(defvar codegeex-extinfo `((sid . ,(uuidgen-4))
                           (ide . "Emacs")
                           (ideVersion . ,emacs-version))
  "The ext field in JSON to be sent to server")

(defun codegeex-debug-invoke (prompt lang begin end)
  "Invoke the codegeex debugger API.

PROMPT is the code content to be debugged. LANG is the programming language name
of the code. The result will be replaced between BEGIN and END in the current
buffer."
  (let* ((url (concat codegeex-endpoint "multilingual_code_bugfix"))
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

(defun codegeex--plist-get (plist &rest keys)
  "Browse PLIST object for each KEYS and return the element
where it stop.
KEYS can be either numbers or properties symbols"
  (let ((res plist))
    (dolist (key keys)
      (cond
       ((symbolp key)
        (setq res (plist-get res key)))
       ((numberp key)
        (setq res (nth key res)))
       (t
        (error "Key '%s' format not supported" key))))
    res))

(defvar-local codegeex--completion-cache nil)
(defvar-local codegeex--completion-idx 0)
(defvar-local codegeex--last-pos nil)

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

;;;###autoload
(defun codegeex-complete ()
  "Get completion at point, showing the completion in an overlay"
  (interactive)
  (setq codegeex--completion-cache nil)
  (setq codegeex--last-pos (point))
  (codegeex-completion--get-completion
   (lambda (json-result)
     (when (equal (point) codegeex--last-pos)
       (let* ((completions
               (cl-remove-if
                (lambda (e)
                  (string= "" (string-trim e)))
                (cl-remove-duplicates
                 (codegeex--plist-get
                  json-result :result :output :code))))
              (completion (if (seq-empty-p completions) nil (seq-elt completions 0))))
         (setq codegeex--completion-cache completions)
         (if completion
             (codegeex-completion--show-completion completion)
           (message "No completion available")))))))

(defun codegeex--cycle-completion (direction)
  "Cycle completion with DIRECTION."
  (let ((completions codegeex--completion-cache))
    (cond ((seq-empty-p completions)
           (message "No completion is available."))
          ((= (length completions) 1)
           (message "Only one completion is available."))
          (t (let ((idx (mod (+ codegeex--completion-idx direction)
                             (length completions))))
               (setq codegeex--completion-idx idx)
               (let ((completion (elt completions idx)))
                 (codegeex-completion--show-completion completion)))))))

;;;###autoload
(defun codegeex-next-completion ()
  "Cycle to next completion."
  (interactive)
  (when (codegeex--overlay-visible)
    (codegeex--cycle-completion 1)))

;;;###autoload
(defun codegeex-previous-completion ()
  "Cycle to previous completion."
  (interactive)
  (when (codegeex--overlay-visible)
    (codegeex--cycle-completion -1)))

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
(defun codegeex-complete-at-point ()
  "Get first completion proposed and insert it at point
Can be used without having to be in `codegeex-mode'"
  (interactive)
  (codegeex-completion--get-completion
   (lambda (json-result)
     (let ((completion (codegeex--plist-get
                        json-result :result :output :code 0)))
       (if completion
         (insert completion)
       (message "No completion available"))))))

;;;###autoload
(defun codegeex-accept-completion (&optional transform-fn)
  "Accept completion. Return t if there is a completion.
Use TRANSFORM-FN to transform completion if provided."
  (interactive)
  (when (codegeex--overlay-visible)
    (let* ((completion (overlay-get codegeex--overlay 'completion))
           (start (overlay-get codegeex--overlay 'start))
           (end (codegeex--overlay-end codegeex--overlay))
           (uuid (overlay-get codegeex--overlay 'uuid))
           (t-completion (funcall (or transform-fn #'identity) completion)))
      (codegeex-clear-overlay)
      (if (eq major-mode 'vterm-mode)
          (progn
            (vterm-delete-region start end)
            (vterm-insert t-completion))
        (delete-region start end)
        (insert t-completion))
      ;; if it is a partial completion
      (when (and (s-prefix-p t-completion completion)
                 (not (s-equals-p t-completion completion)))
        (codegeex--set-overlay-text (codegeex--get-overlay) (s-chop-prefix t-completion completion)))
      t)))

;; minor mode

(defvar codegeex--post-command-timer nil)

(defcustom codegeex-disable-predicates nil
  "A list of predicate functions with no argument to disable Codegeex.
Codegeex will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'codegeex)

(defcustom codegeex-enable-predicates '(evil-insert-state-p codegeex--buffer-changed)
  "A list of predicate functions with no argument to enable Codegeex.
Codegeex will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'codegeex)

(defcustom codegeex-disable-display-predicates nil
  "A list of predicate functions with no argument to disable Codegeex.
Codegeex will not show completions if any predicate returns t."
  :type '(repeat function)
  :group 'codegeex)

(defcustom codegeex-enable-display-predicates nil
  "A list of predicate functions with no argument to enable Codegeex.
Codegeex will show completions only if all predicates return t."
  :type '(repeat function)
  :group 'codegeex)

(defmacro codegeex--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
                    (if (functionp pred) (funcall pred) t))
                  ,enable)
        (cl-notany (lambda (pred)
                     (if (functionp pred) (funcall pred) nil))
                   ,disable)))

(defun codegeex--satisfy-trigger-predicates ()
  "Return t if all trigger predicates are satisfied."
  (codegeex--satisfy-predicates codegeex-enable-predicates codegeex-disable-predicates))

(defun codegeex--satisfy-display-predicates ()
  "Return t if all display predicates are satisfied."
  (codegeex--satisfy-predicates codegeex-enable-display-predicates codegeex-disable-display-predicates))

(defvar codegeex-mode-map (make-sparse-keymap)
  "Keymap for Codegeex minor mode.
Use this for custom bindings in `codegeex-mode'.")

(defun codegeex--mode-enter ()
  "Set up codegeex mode when entering."
  (add-hook 'post-command-hook #'codegeex--post-command nil 'local))

(defun codegeex--mode-exit ()
  "Clean up codegeex mode when exiting."
  (remove-hook 'post-command-hook #'codegeex--post-command 'local))

(defun codegeex--posn-advice (&rest args)
  "Remap posn if in codegeex-mode."
  (when codegeex-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and codegeex--real-posn
                 (eq pos (car codegeex--real-posn)))
        (cdr codegeex--real-posn)))))

(defun codegeex--post-command ()
  "Complete in `post-command-hook' hook."
  (when (and this-command
             (not (and (symbolp this-command)
                       (or
                        (s-starts-with-p "codegeex-" (symbol-name this-command))
                        (member this-command codegeex-clear-overlay-ignore-commands)
                        (codegeex--self-insert this-command)))))
    (codegeex-clear-overlay)
    (when codegeex--post-command-timer
      (cancel-timer codegeex--post-command-timer))
    (setq codegeex--post-command-timer
          (run-with-idle-timer codegeex-idle-delay
                               nil
                               #'codegeex--post-command-debounce
                               (current-buffer)))))

(defun codegeex--self-insert (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue. COMMAND is the
command that triggered `post-command-hook'."
  (when (and (eq command 'self-insert-command)
             (codegeex--overlay-visible)
             (codegeex--satisfy-display-predicates))
    (let* ((ov codegeex--overlay)
           (completion (overlay-get ov 'completion)))
      ;; The char just inserted is the next char of completion
      (when (eq last-command-event (elt completion 0))
        (if (= (length completion) 1)
            ;; If there is only one char in the completion, accept it
            (codegeex-accept-completion)
          (codegeex--set-overlay-text ov (substring completion 1)))))))

(defun codegeex--post-command-debounce (buffer)
  "Complete in BUFFER."
  (when (and (buffer-live-p buffer)
             (equal (current-buffer) buffer)
             codegeex-mode
             (codegeex--satisfy-trigger-predicates))
    (codegeex-complete)))

;;;###autoload
(define-global-minor-mode global-codegeex-mode
  codegeex-mode codegeex-mode)

;;;###autoload
(define-minor-mode codegeex-mode
  "Minor mode for Codegeex."
  :init-value nil
  :lighter " Codegeex"
  (codegeex-clear-overlay)
  (advice-add 'posn-at-point :before-until #'codegeex--posn-advice)
  (if codegeex-mode
      (codegeex--mode-enter)
    (codegeex--mode-exit)))

(provide 'codegeex)
;;; codegeex.el ends here
