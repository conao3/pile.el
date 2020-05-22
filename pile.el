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
              (magit-insert-section (pilebuf)))))
    (unless nodisplay
      (switch-to-buffer-other-window (current-buffer)))
    (current-buffer)))

(defun pile--insert-section (command)
  "Insert new section for COMMAND.
See `magit-process-insert-section'."
  (let ((inhibit-read-only t)
        (program (car command))
        (args (cdr command)))
    (with-current-buffer (pile-buffer 'nodisplay)
      (goto-char (point-max))
      (let ((magit-insert-section--parent pile-root-section))
        (magit-insert-section (process)
          (insert (propertize (file-name-nondirectory program)
                              'font-lock-face 'magit-section-heading) " ")
          (insert (propertize (mapconcat #'shell-quote-argument args " ")
                              'font-lock-face 'magit-section-heading))
          (magit-insert-heading)
          (insert "\n\n"))))))

(defun pile--prepare-eshell-marker ()
  "Prepare eshel marker for `current-buffer', `point'."
  (set (make-local-variable 'eshell-last-input-start) (point-marker))
  (set (make-local-variable 'eshell-last-input-end) (point-marker))
  (set (make-local-variable 'eshell-last-output-start) (point-marker))
  (set (make-local-variable 'eshell-last-output-end) (point-marker))
  (set (make-local-variable 'eshell-last-output-block-begin) (point)))

(defun pile--promise-make-process-with-handler (command &optional handler merge-stderr)
  "Return promise to make new asynchronous COMMAND.

Arguments:
  - PROGRAM is shell commands list of string.
  - HANDLER is function, called with process object after program is invoked.
  - MERGE-STDERR is boolean, whether merge stdout and stderr

Resolve:
  - A list like as (stdout stderr) when process finish with exitcode 0.
    stdout and stderr are string.

Reject:
  - A list like as (event stdout stderr) when process doesn't finish exitcode 0.
    event, stdout and stderr are string.
    The event is documented at https://www.gnu.org/software/emacs/manual/html_node/elisp/Sentinels.html"
  (promise-new
   (lambda (resolve reject)
     (let* ((program (car command))
            (stdout (generate-new-buffer (concat "*" program "-stdout*")))
            (stderr (unless merge-stderr
                      (generate-new-buffer (concat "*" program "-stderr*"))))
            (stderr-pipe (unless merge-stderr
                           (make-pipe-process
                            :name (concat "*" program "-stderr-pipe*")
                            :noquery t
                            ;; use :filter instead of :buffer, to get rid of "Process Finished" lines
                            :filter (lambda (_ output)
                                      (with-current-buffer stderr
                                        (insert output))))))
            (cleanup (lambda ()
                       (kill-buffer stdout)
                       (unless merge-stderr
                         (delete-process stderr-pipe)
                         (kill-buffer stderr)))))
       (condition-case err
           (let ((proc (if merge-stderr
                           (make-process :name program
                                         :buffer stdout
                                         :command command)
                         (make-process :name program
                                       :buffer stdout
                                       :command command
                                       :stderr stderr-pipe))))
             (set-process-sentinel
              proc
              (lambda (_process event)
                (unwind-protect
                    (let ((stdout-str (with-current-buffer stdout
                                        (buffer-string)))
                          (stderr-str (unless merge-stderr
                                        (with-current-buffer stderr
                                          (buffer-string)))))
                      (if (string= event "finished\n")
                          (funcall resolve (list stdout-str stderr-str))
                        (funcall reject (list event stdout-str stderr-str))))
                  (funcall cleanup))))
             (when handler
               (funcall handler proc)))
         (error (funcall cleanup)
                (signal (car err) (cdr err))))))))

(defun pile--output-filter (buf proc string)
  "Send the output to BUF from PROC (STRING) to the interactive display.
This is done after all necessary filtering has been done."
  (let ((oprocbuf buf)
        (inhibit-read-only t)
        (inhibit-point-motion-hooks t)
        (inhibit-modification-hooks t))
    (let ((functions eshell-preoutput-filter-functions))
      (while (and functions string)
        (setq string (funcall (car functions) string))
        (setq functions (cdr functions))))
    (if (and string oprocbuf (buffer-name oprocbuf))
        (let (opoint obeg oend)
          (with-current-buffer oprocbuf
            (setq opoint (point))
            (setq obeg (point-min))
            (setq oend (point-max))
            (let ((buffer-read-only nil)
                  (nchars (length string))
                  (ostart nil))
              (widen)
              (goto-char eshell-last-output-end)
              (setq ostart (point))
              (if (<= (point) opoint)
                  (setq opoint (+ opoint nchars)))
              (if (< (point) obeg)
                  (setq obeg (+ obeg nchars)))
              (if (<= (point) oend)
                  (setq oend (+ oend nchars)))
              ;; Let the ansi-color overlay hooks run.
              (let ((inhibit-modification-hooks nil))
                (insert-before-markers
                 (propertize string 'magit-section
                             (process-get proc 'section))))
              (if (= (window-start) (point))
                  (set-window-start (selected-window)
                                    (- (point) nchars)))
              (if (= (point) eshell-last-input-end)
                  (set-marker eshell-last-input-end
                              (- eshell-last-input-end nchars)))
              (set-marker eshell-last-output-start ostart)
              (set-marker eshell-last-output-end (point))
              (force-mode-line-update))
            (narrow-to-region obeg oend)
            (goto-char opoint)
            (eshell-run-output-filters))))))

(defvar pile-process nil)

(defun pile--promise-make-process (section command)
  "Run COMMAND and output at SECTION."
  (let* ((ptr (oref section end))
         (buf (marker-buffer ptr)))
    (with-current-buffer buf
      (goto-char (- ptr 1))
      (pile--prepare-eshell-marker)
      (pile--promise-make-process-with-handler
       command
       (lambda (proc)
         (setq pile-process proc)
         (oset section value proc)
         (process-put proc 'section section)
         (set-process-filter
          proc
          (lambda (proc string)
            (pile--output-filter buf proc string))))
       'merge))))


;;; main

(async-defun pile-make-process (command)
  "Exec COMMAND via `shell-command' async and output `pile-buffer'.
See `magit-call-process'."
  (let ((section (pile--insert-section command)))
    (await (pile--promise-make-process section command))))

(define-derived-mode pile-section-mode magit-section-mode "Pile"
  "Major-mode for pile buffer."
  :group 'pile)

(provide 'pile)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; pile.el ends here
