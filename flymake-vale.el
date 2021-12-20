;;; flymake-vale.el --- flymake integration for vale -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: December 19, 2021
;; Modified: December 19, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/new
;; Package-Requires: ((emacs "24.4") (flymake "0.22") (let-alist "1.0.4"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Description:
;;
;; This provides flymake integration for vale. It allows flymake to
;; use vale to provide natural language linting.
;;
;; Basic usage:
;;
;;  (require 'flymake-vale)
;;  (flymake-vale-setup)
;;
;;; Code:

(require 'flymake)
(require 'let-alist)

(defgroup flymake-vale nil
  "Variables related to flymake-vale."
  :prefix "flymake-vale-"
  :group 'tools)

(defcustom flymake-vale-program "vale"
  "The vale executable to use."
  :type '(string)
  :group 'flymake-vale)

(defconst flymake-vale-modes '(text-mode markdown-mode rst-mode org-mode))

(defcustom flymake-vale-output-buffer "*flymake-vale*"
  "Buffer where tool output gets written."
  :type '(string)
  :group 'flymake-vale)

(defvar-local flymake-vale--output nil
  "Copy of the JSON output.")

(defvar-local flymake-vale--source-buffer nil
  "Current buffer we are currently using for grammar check.")

(defvar flymake-vale--report-fnc nil
  "Record report function/execution.")

(defconst flymake-vale--level-map
  '(("error" . :error)
    ("warning" . :warning)
    ("suggestion" . :note)))

;;; Util

(defun flymake-vale--pos-at-line-col (l c)
  (save-excursion
    (goto-char (point-min))
    (goto-line l)
    (move-to-column c)
    (point)))

(defun flymake-vale--check-all (errors)
  "Parse ISSUES into flymake error structs."
  (let (check-list)
    (dolist (error errors)
      (let-alist error
        (push (flymake-make-diagnostic
               flymake-vale--source-buffer
               (flymake-vale--pos-at-line-col .Line (- (car .Span) 1))
               (flymake-vale--pos-at-line-col .Line   (cadr .Span))
               (assoc-default .Severity flymake-vale--level-map 'string-equal 'error)
               (concat .Message " [Vale]"))
              check-list)))
    check-list))

(defun flymake-vale--output-to-errors (output)
  "Parse the full JSON output of vale, OUTPUT, into a sequence of flymake error structs."
  (let* ((json-array-type 'list)
         (full-results (json-read-from-string output))

         ;; Get the list of errors for each file.
         (result-vecs (mapcar 'cdr full-results))

         ;; Chain all of the errors together. The point here, really, is that we
         ;; don't expect results from more than one file, but we should be
         ;; prepared for the theoretical possibility that the errors are somehow
         ;; split across multiple files. This is basically a punt in lieu of
         ;; more information.
         (errors (apply 'append (mapcar 'cdr full-results))))
    (flymake-vale--check-all errors)))

(defun flymake-vale--handle-finished (callback buf)
  "Parse the contents of the output buffer into flymake error structures. 
Passing the results and source BUF to CALLBACK."
  (let* ((output (with-current-buffer flymake-vale-output-buffer
                   (buffer-string)))
         (errors (flymake-vale--output-to-errors output))
         (region (with-current-buffer buf
                   (cons (point-min) (point-max)))))
    ;; Fill in the rest of the error struct database
    (funcall callback errors :region region)))

(defun flymake-vale--normal-completion? (event)
  (or  (string-equal event "finished\n")
       (string-match "exited abnormally with code 1.*" event)))

;;; Flymake

(defun flymake-vale--start ()
  "Run vale on the current buffer's contents."
  ;; Clear the output buffer
  (with-current-buffer (get-buffer-create flymake-vale-output-buffer)
    (read-only-mode 0)
    (erase-buffer))

  (let* ((process-connection-type nil)
         (proc (start-process "flymake-vale-process"
                              flymake-vale-output-buffer
                              flymake-vale-program
                              "--output"
                              "JSON")))
    (let ((callback flymake-vale--report-fnc)
          (buf (current-buffer)))
      (set-process-sentinel
       proc
       #'(lambda (process event)
           (when (flymake-vale--normal-completion? event)
             (flymake-vale--handle-finished callback buf)))))

    (process-send-region proc (point-min) (point-max))
    (process-send-eof proc)))

(defun flymake-vale--checker (report-fn &rest _args)
  "Diagnostic checker function with REPORT-FN."
  (setq flymake-vale--report-fnc report-fn)
  (setq flymake-vale--source-buffer (current-buffer))
  (flymake-vale--start))

;;; Entry

;;;###autoload
(defun flymake-vale-load ()
  "Convenience function to setup flymake-vale.
This adds the vale checker to the list of flymake diagnostic functions."
  (add-hook 'flymake-diagnostic-functions #'flymake-vale--checker nil t))

;;;###autoload
(defun flymake-vale-maybe-load ()
  "Call `flymake-vale-load' if this file appears to be check for grammar."
  (interactive)
  (when (memq major-mode flymake-vale-modes)
    (flymake-vale-load)))

(provide 'flymake-vale)
;;; flymake-vale.el ends here
