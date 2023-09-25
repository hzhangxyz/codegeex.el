;;; codegeex-overlay.el --- Codegeex for Emacs       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Samuel D

;; Author: Samuel D <samueld@mailo.com>
;; Keywords: convenience

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

;; Taken from copilot.el
;; Create and handle the overlay shown during
;; buffer completion process

;;; Code:

(defface codegeex-overlay-face
  '((t :inherit shadow))
  "Face for codegeex overlay.")

(defvar-local codegeex--overlay nil
  "Overlay for Codegeex completion.")

(defvar-local codegeex--real-posn nil
  "Posn information without overlay.
To work around posn problems with after-string property.")

(defconst codegeex-overlay-completion-map (make-sparse-keymap)
  "Keymap for Codegeex completion overlay.")

(defun codegeex--get-overlay ()
  "Create or get overlay for Codegeex."
  (unless (overlayp codegeex--overlay)
    (setq codegeex--overlay (make-overlay 1 1 nil nil t))
    (overlay-put codegeex--overlay
                 'keymap codegeex-overlay-completion-map)
    (overlay-put codegeex--overlay 'priority 100))
  codegeex--overlay)

(defun codegeex--overlay-end (ov)
  "Return the end position of overlay OV."
  (- (line-end-position) (overlay-get ov 'tail-length)))

(defun codegeex--set-overlay-text (ov completion)
  "Set overlay OV with COMPLETION."
  (move-overlay ov (point) (line-end-position))
  (let* ((tail (buffer-substring (codegeex--overlay-end ov) (line-end-position)))
         (p-completion (concat (propertize completion 'face 'codegeex-overlay-face)
                               tail)))
    (if (eolp)
        (progn
          (overlay-put ov 'after-string "") ; make sure posn is correct
          (setq codegeex--real-posn (cons (point) (posn-at-point)))
          (put-text-property 0 1 'cursor t p-completion)
          (overlay-put ov 'display "")
          (overlay-put ov 'after-string p-completion))
      (overlay-put ov 'display (substring p-completion 0 1))
      (overlay-put ov 'after-string (substring p-completion 1)))
    (overlay-put ov 'completion completion)
    (overlay-put ov 'start (point))))

(defun codegeex--display-overlay-completion-1 (completion start end)
  ;; Create overlay and set its text.
  )

(defun codegeex--display-overlay-completion (completion start end)
  "Show COMPLETION between START and END."
  (setq end start)
  (codegeex-clear-overlay)
  (message "%s %d %d" completion start end)
  (when (and (s-present-p completion)
             (or (= start (point))      ; up-to-date completion
                 (and (< start (point)) ; special case for removing indentation
                      (s-blank-p (s-trim (buffer-substring-no-properties start (point)))))))
    (goto-char start)                   ; indentation
    (let ((ov (codegeex--get-overlay)))
      (overlay-put ov 'tail-length (- (line-end-position) end))
      (codegeex--set-overlay-text ov completion))))

(defun codegeex-clear-overlay ()
  "Clear Codegeex overlay"
  (interactive)
  (when (codegeex--overlay-visible)
    (delete-overlay codegeex--overlay)
    (setq codegeex--real-posn nil)))

(defsubst codegeex--overlay-visible ()
  "Return whether the `codegeex--overlay' is avaiable."
  (and (overlayp codegeex--overlay)
       (overlay-buffer codegeex--overlay)))


(provide 'codegeex-overlay)
;;; codegeex-overlay.el ends here
