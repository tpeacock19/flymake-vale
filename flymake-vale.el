;;; flymake-vale.el --- flymake integration for vale -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: December 19, 2021
;; Modified: December 19, 2021
;; Version: 0.0.2
;; Keywords:
;; Homepage: https://github.com/tpeacock19/new
;; Package-Requires: ((emacs "27.1") (flymake "0.22")
;;  (let-alist "1.0.4") (compat "29.1.4.4"))
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

(require 'compat)
(eval-when-compile
  (require 'let-alist))
(require 'json)
(require 'flymake)

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

(defvar-local flymake-vale--buf-checked '(nil . 0)
  "Current state of the source buffer.
If the entire buffer has been checked this will be t")

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

(defun flymake-vale--check-all (errors start)
  "Parse ERRORS into flymake error structs."
  (let (check-list)
    (dolist (error errors)
      (let-alist error
        (let ((check (split-string .Check "\\."))
              (pos (save-excursion
                     (goto-char start)
                     (unless (and (eq .Line 1)
                                  (not (bolp)))
                       (forward-line (1- .Line)))
                     (forward-char (1- (car .Span)))
                     (cons (point) (+ (point) (length .Match))))))
          (push (flymake-make-diagnostic
                 flymake-vale--source-buffer
                 (car pos) (cdr pos)
                 (assoc-default .Severity flymake-vale--level-map
                                'string-equal 'error)
                 (format "%s [vale:%s:%s]" .Message
                         (car check) (cadr check)))
                check-list))))
    check-list))

(defun flymake-vale--output-to-errors (output source start)
  "Parse the full JSON OUTPUT of vale.
Converts output into a sequence of flymake error structs."
  (let* ((json-array-type 'list)
         (json-out (unless (string-prefix-p "{" output)
                     (substring output (string-match "\n{" output))))
         (full-results (json-read-from-string (or json-out output)))
         (errors (apply 'append (mapcar 'cdr full-results))))
    (with-current-buffer source (flymake-vale--check-all errors start))))

(defun flymake-vale--proc-error-p (output proc source report-fn)
  "Check if Vale returned in error in OUTPUT."
  (pcase output
    ;; empty string most likely means process was closed by a new
    ;; flymake check
    ((pred string-empty-p) 'obsolete)
    (_ (let-alist (ignore-errors (json-read-from-string output))
         (when (and (stringp .Code)
                    (string-match-p  "E[0-9]\\{3\\}$" .Code))
           (string-replace  "\n" " " .Text))))))

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

(defun flymake-vale--start (report-fn source state start end)
  "Run vale on the current buffer's contents."
  (setq
   flymake-vale--proc
   (make-process
    :name "flymake-vale-process"
    :noquery t :connection-type 'pipe
    :buffer (generate-new-buffer flymake-vale-output-buffer)
    :command `(,flymake-vale-program
               "--output" "JSON" ,@(flymake-vale--build-args))
    :sentinel
    (lambda (proc _event)
      (when (memq (process-status proc) '(signal stop))
	(flymake-log :debug "Skipping obsolete check"))
      (when (eq 'exit (process-status proc))
        (let* ((proc-current (eq proc flymake-vale--proc))
	       (proc-buf (process-buffer proc))
	       (output (with-current-buffer proc-buf
			 (buffer-string)))
	       (err (flymake-vale--proc-error-p output proc source
						report-fn)))
	  (unwind-protect
	      (cond
	       ((and proc-current (stringp err))
		(funcall report-fn :panic :explanation err))
	       (proc-current
		(funcall report-fn (flymake-vale--output-to-errors
				    output source start)
                         :region (cons start end))
                (unless (car flymake-vale--buf-checked)
                  (setf (cdr flymake-vale--buf-checked) end))
                (when (eq end (point-max))
                  (setf (car flymake-vale--buf-checked) t))))
	    (when (buffer-name proc-buf)
	      (kill-buffer proc-buf))
	    (when proc-current
	      (setq-local flymake-vale--proc nil))))))))
  (process-send-region flymake-vale--proc start end)
  (process-send-eof flymake-vale--proc))

(defun flymake-vale--sentence-point (pt arg)
  "Wrapper for `forward-sentence-function'.
A positive ARG will return beginning point ARG number sentence preceding PT.
A positive ARG will return end point ARG number sentence following PT."
  (when pt
    (save-excursion
      sentence-end-double-space
      (goto-char pt)
      (funcall (or (and (boundp 'forward-sentence-function)
                        forward-sentence-function)
                   #'forward-sentence)
               arg))))

(defun flymake-vale--setup ()
  "Used to reset the checked state of the current buffer."
  (setf (car flymake-vale--buf-checked) nil
        (cdr flymake-vale--buf-checked) 0
        flymake-vale--proc nil))

(defun flymake-vale--check-state (args)
  "Determine what region has changed and needs to be checked.
Function acceps ARGS sent from `flymake' describing potential changes."
  (if (plist-member args :recent-changes)
      (let ((changes (or (plist-get args :recent-changes) 'none))
            (start (or (flymake-vale--sentence-point
                        (plist-get args :changes-start) -1)
		       (point-min)))
            (end (or (flymake-vale--sentence-point
		      (plist-get args :changes-end) 1)
                     (point-max))))
        (list changes start end))
    (if (car flymake-vale--buf-checked)
        (list 'save
	      (flymake-vale--sentence-point (point) -2)
	      (flymake-vale--sentence-point (point) 2))
      (list 'start (max (point-min) (cdr flymake-vale--buf-checked))
            (point-max)))))

(defun flymake-vale--checker (report-fn &rest args)
  "Diagnostic checker function with REPORT-FN."
  (setq-local flymake-vale--source-buffer (current-buffer))
  ;; kill and cleanup any ongoing processes. This is meant to be more
  ;; performant instead of checking when the vale process finishes.
  (when-let ((proc flymake-vale--proc)
	     (proc-buf (process-buffer flymake-vale--proc)))
    (when (process-live-p proc)
      (delete-process flymake-vale--proc))
    (when (buffer-name proc-buf)
      (kill-buffer proc-buf))
    (setq-local flymake-vale--proc nil))
  (pcase-let* ((a args)
	       (`(,state ,start ,end)
		(flymake-vale--check-state a)))
    (flymake-vale--start report-fn
			 flymake-vale--source-buffer
			 state start end)))

;;; Entry

;;;###autoload
(defun flymake-vale-load ()
  "Convenience function to setup flymake-vale.
This adds the vale checker to the list of flymake diagnostic
functions."
  (add-hook 'flymake-diagnostic-functions #'flymake-vale--checker
	    nil t)
  (add-hook 'flymake-mode-hook #'flymake-vale--setup nil t))

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
