;;; flymake-vale.el --- flymake integration for vale -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: December 19, 2021
;; Modified: December 19, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/new
;; Package-Requires: ((emacs "24.4") (flymake "0.22")
;;  (let-alist "1.0.4"))
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
;; This provides flymake integration for vale. It allows flymake to
;; use vale to provide natural language linting.
;;
;;; Code:

(require 'flymake)
(require 'let-alist)
(require 'json)

(defgroup flymake-vale nil
  "Variables related to flymake-vale."
  :prefix "flymake-vale-"
  :group 'tools)

(defcustom flymake-vale-program (executable-find "vale")
  "The Vale executable to use."
  :type '(string)
  :group 'flymake-vale)

(defcustom flymake-vale-program-args nil
  "Extra arguments to pass when running Vale."
  :type '(string)
  :group 'flymake-vale)

(defcustom flymake-vale-modes '(text-mode latex-mode org-mode
                                          markdown-mode message-mode)
  "List of major mode that work with Vale."
  :type 'list
  :group 'flymake-vale)

(defcustom flymake-vale-mode-file-exts '((markdown-mode . "md")
                                         (gfm-mode . "md")
                                         (html-mode . "html")
                                         (rst-mode . "rst")
                                         (adoc-mode . "asciidoc")
                                         (xml-mode . "xml")
                                         (org-mode . "org")
                                         (text-mode . "txt")
                                         (c-mode . "c")
                                         (c++-mode . "cpp")
                                         (css-mode . "css")
                                         (go-mode . "go")
                                         (haskell-mode . "hs")
                                         (java-mode . "java")
                                         (less-mode . "less")
                                         (lua-mode . "lua")
                                         (perl-mode . "pl")
                                         (php-mode . "php")
                                         (python-mode . "py")
                                         (ess-r-mode . "r")
                                         (ruby-mode . "rb")
                                         (sass-mode . "sass")
                                         (scala-mode . "scala")
                                         (swift-mode . "swift"))
  "An alist of major-modes with associated file extensions."
  :type 'list
  :group 'flymake-vale)

(defcustom flymake-vale-output-buffer " *flymake-vale*"
  "Buffer where tool output gets written."
  :type '(string)
  :group 'flymake-vale)

(defvar-local flymake-vale--output nil
  "Copy of the JSON output.")

(defvar-local flymake-vale--source-buffer nil
  "Current buffer we are currently using for grammar check.")

(defvar-local flymake-vale--proc nil
  "A buffer-local variable handling the vale process for flymake.")

(defvar-local flymake-vale-file-ext nil
  "A buffer-local variable providing file extension to Vale.
The extension is necessary for Vale's format-sensitive parsing.")

(defvar flymake-vale--report-fnc nil
  "Record report function/execution.")

(defconst flymake-vale--level-map
  '(("error" . :error)
    ("warning" . :warning)
    ("suggestion" . :note)))

;;; Util

(defun flymake-vale--match-at-line (line match pos)
  (with-current-buffer flymake-vale--source-buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (- line 1))
      (re-search-forward match)
      (cond
       ((eq 'start pos)
        (match-beginning 0))
       ((eq 'end pos)
        (match-end 0))))))

(defun flymake-vale--check-all (errors)
  "Parse ISSUES into flymake error structs."
  (let (check-list)
    (dolist (error errors)
      (let-alist error
        (let ((check (split-string .Check "\\.")))
          (push (flymake-make-diagnostic
                 flymake-vale--source-buffer
                 (flymake-vale--match-at-line .Line .Match 'start)
                 (flymake-vale--match-at-line .Line .Match 'end)
                 (assoc-default .Severity flymake-vale--level-map
                                'string-equal 'error)
                 (format "%s [vale:%s:%s]" .Message
                         (car check) (cadr check)))
                check-list))))
    check-list))

(defun flymake-vale--output-to-errors (output)
  "Parse the full JSON OUTPUT of vale.
Converts output into a sequence of flymake error structs."
  (let* ((json-array-type 'list)
         (json-out (unless (string-prefix-p "{" output)
                     (substring output (string-match "\n{" output))))
         (full-results (json-read-from-string (or json-out output)))
         (errors (apply 'append (mapcar 'cdr full-results))))
    (flymake-vale--check-all errors)))

(defun flymake-vale--handle-finished (callback buf)
  "Parse contents and pass results and source BUF to
 CALLBACK."
  (let* ((output (with-current-buffer flymake-vale-output-buffer
                   (buffer-string)))
         (region (with-current-buffer buf
                   (cons (point-min) (point-max)))))
    (if-let ((errors (flymake-vale--proc-error-p output)))
        (funcall callback :panic :explanation errors)
      (funcall callback (flymake-vale--output-to-errors output)
               :region region))
    (kill-buffer (process-buffer flymake-vale--proc))))

(defun flymake-vale--proc-error-p (output)
  (let-alist (ignore-errors (json-read-from-string output))
    (when (and (stringp .Code)
               (string-match-p  "E[0-9]\\{3\\}$" .Code))
      (string-replace  "\n" " " .Text))))

(defun flymake-vale--detect-extension ()
  "Attempt to detect a file extension related to the buffer we
 wish to check, either using the file extension, or with the
`flymake-vale-file-ext' variable."
  (let* ((f (buffer-file-name flymake-vale--source-buffer))
         (ext (or flymake-vale-file-ext
                  (and f (file-name-extension f))
                  (alist-get (buffer-local-value
                              'major-mode flymake-vale--source-buffer)
                             flymake-vale-mode-file-exts))))
    (list (if ext (concat "--ext=." ext) ""))))

(defun flymake-vale--build-args ()
  "Build arguments to pass to the vale executable."
  (append flymake-vale-program-args
          (flymake-vale--detect-extension)))

;;; Flymake

(defun flymake-vale--start ()
  "Run vale on the current buffer's contents."
  ;; Clear the output buffer
  (with-current-buffer (get-buffer-create flymake-vale-output-buffer)
    (read-only-mode 0)
    (erase-buffer))
  (when (process-live-p flymake-vale--proc)
    (kill-process flymake-vale--proc))
  (let* ((process-connection-type nil)
         (callback flymake-vale--report-fnc)
         (buf (current-buffer))
         (proc (apply #'start-process
                      "flymake-vale-process"
                      flymake-vale-output-buffer
                      flymake-vale-program
                      "--output"
                      "JSON"
                      (flymake-vale--build-args))))
    (setq flymake-vale--proc proc)
    (set-process-sentinel
     proc
     #'(lambda (p event)
         (when (eq 'exit (process-status p))
           (if (eq p flymake-vale--proc)
               (flymake-vale--handle-finished callback buf)
             (flymake-log :warning "Canceling obsolete check %s"
                          p)))))
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
This adds the vale checker to the list of flymake diagnostic
functions."
  (add-hook 'flymake-diagnostic-functions #'flymake-vale--checker
            nil t))

;;;###autoload
(defun flymake-vale-maybe-load ()
  "Call `flymake-vale-load' when file should be checked for
 grammar."
  (interactive)
  (when (memq major-mode flymake-vale-modes)
    (flymake-vale-load)))

(provide 'flymake-vale)
;;; flymake-vale.el ends here

;; Local Variables:
;; fill-column: 72
;; emacs-lisp-docstring-fill-column: 65
;; End:
