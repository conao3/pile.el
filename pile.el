;;; pile.el --- Pile information using magit-section  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (magit-section "2.90") (async-await "1.1"))
;; URL: https://github.com/conao3/pile.el

;; This program is free software: you can redistribute it and/or modify
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

;; Pile information using magit-section.


;;; Code:

(require 'subr-x)
(require 'esh-mode)
(require 'magit-section)
(require 'async-await)

(defgroup pile nil
  "Pile information using magit-section."
  :prefix "pile-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/pile.el"))


;;; functions

(defvar-local pile-root-section nil
  "Root magit-section in the current buffer.")

(defun pile-buffer (&optional nodisplay)
  "Return pile buffer and display if not NODISPLAY.
See `magit-process-buffer'."
  (interactive)
  (with-current-buffer (get-buffer-create "*pile*")
    (unless pile-root-section
      (pile-section-mode)
      (let ((inhibit-read-only t))
        (setq pile-root-section
              (magit-insert-section (pilebuf)
                (insert "\n")))))
    (unless nodisplay
      (switch-to-buffer-other-window (current-buffer)))
    (current-buffer)))

(defun pile--insert-section (program args)
  "Insert new section for PROGRAM with ARGS.
See `magit-process-insert-section'."
  (let ((inhibit-read-only t)
        (magit-insert-section--parent magit-root-section)
        (magit-insert-section--oldroot nil))
    (goto-char (1- (point-max)))
    (prog1 (magit-insert-section (process)
             (insert (propertize (file-name-nondirectory program)
                                 'font-lock-face 'magit-section-heading) " ")
             (insert (propertize (mapconcat #'shell-quote-argument args " ")
                                 'font-lock-face 'magit-section-heading))
             (magit-insert-heading)
             (insert "\n"))
      (backward-char 1))))

(defun pile--prepare-eshell-marker ()
  "Prepare eshel marker for `current-buffer', `point'."
  (set (make-local-variable 'eshell-last-input-start) (point-marker))
  (set (make-local-variable 'eshell-last-input-end) (point-marker))
  (set (make-local-variable 'eshell-last-output-start) (point-marker))
  (set (make-local-variable 'eshell-last-output-end) (point-marker))
  (set (make-local-variable 'eshell-last-output-block-begin) (point)))

(defvar pile-process nil)

(defun pile--promise-make-process (section program args)
  "Run PROGRAM with ARGS and output at SECTION."
  (let* ((ptr (oref section end))
         (buf (marker-buffer ptr)))
    (with-current-buffer buf
      (goto-char (- ptr 1))
      (pile--prepare-eshell-marker)
      (apply
       #'promise:make-process-with-handler
       program
       (lambda (proc)
         (setq pile-process proc)
         (set-process-filter
          proc
          (lambda (proc string)
            (let ((inhibit-read-only t))
              (eshell-output-filter proc string)))))
       args))))


;;; main

(async-defun pile-make-process (program &rest args)
  "Exec PROGRAM with ARGS via `shell-command' async and output `pile-buffer'.
See `magit-call-process'."
  (let ((section (pile--insert-section program args)))
    (await (pile--promise-make-process section program args))))

(define-derived-mode pile-section-mode magit-section-mode "Pile"
  "Major-mode for pile buffer."
  :group 'pile)

(provide 'pile)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; pile.el ends here
