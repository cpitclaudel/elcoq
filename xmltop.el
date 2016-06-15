;;; xmltop.el --- XMLTop REPL in Emacs               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Clément Pit-Claudel

;; Author: Clément Pit--Claudel <clement.pitclaudel@live.com>
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This lets you talk to Coq's XML API, for experimentation purposes.
;;
;; To use, make sure to add (setq xmltop-executable-path "/path/to/coqtop") to your
;; .emacs.

;;; Code:

(require 'comint)
(require 'xml)

(defvar xmltop-executable-path "/build/coq/bin/coqtop.byte"
  "Path to coqtop.")

(defvar-local xmltop--accumulator nil
  "List of strings accumulated from xmltop in reverse order.")

(defun xmltop--format (response)
  "Read RESPONSE into a sexp and return a pretty-printed, indented copy."
  (with-temp-buffer
    (save-excursion (insert (pp-to-string response)))
    (while (re-search-forward "^\\(.\\)" nil t)
      (replace-match "  \\1" t))
    (buffer-string)))

(defconst xmltop-entities-alist '(("nbsp" . " ")))

(defun xmltop--read-tags (str)
  "Read tags in STR."
  (with-temp-buffer ;; FIXME performance
    (insert str)
    (goto-char (point-min))
    (let ((tags nil)
          (end (point))
          (xml-entity-alist (append xml-entity-alist
                                    xmltop-entities-alist)))
      (ignore-errors
        (while t
          (push (xml-parse-tag-1) tags)
          (setq end (point))))
      (cons (nreverse tags) (buffer-substring end (point-max))))))

(defun xmltop--preoutput-filter (string)
  "Accumulate STRING, returning full responses."
  (let ((full (concat (or xmltop--accumulator "") string)))
    (pcase (xmltop--read-tags full)
      (`(,messages . ,leftover)
       (setq xmltop--accumulator leftover)
       (mapconcat #'xmltop--format messages "")))))

(defconst xmltop--font-lock-keywords
  '(("([A-Z]\\(\\w\\|\\s_\\|\\\\.\\)+\\(\\s-\\|\n\\)" . font-lock-function-name-face)
    ("(\\(\\w\\|\\s_\\|\\\\.\\)+\\(\\s-\\|\n\\)" . font-lock-variable-name-face)
    ("\\_<nil\\_>" . font-lock-builtin-face))
  "Font lock pairs for `xmltop-mode'.")

(defvar xmltop-mode-syntax-table lisp--mode-syntax-table
  "Syntax table for `xmltop-mode'.")

(define-derived-mode xmltop-mode comint-mode "XMLTop"
  "Major mode for interacting with XMLTop.

Output is accumulated and printed once a full message has been received."
  (setq comint-process-echoes nil)
  (setq comint-use-prompt-regexp nil)
  (setq font-lock-defaults '(xmltop--font-lock-keywords))
  (when (fboundp 'rainbow-delimiters-mode) (rainbow-delimiters-mode))
  (add-hook 'comint-preoutput-filter-functions #'xmltop--preoutput-filter nil t))

(defun xmltop--args ()
  "Compute xmltop arguments."
  `("-ideslave" "-main-channel" "stdfds"))

(defun xmltop ()
  "Launch xmltop."
  (interactive)
  (let ((buffer (get-buffer-create (generate-new-buffer-name "*XMLtop*"))))
    (pop-to-buffer buffer)
    (apply 'make-comint-in-buffer "XMLTop"
           buffer xmltop-executable-path nil (xmltop--args))
    (xmltop-mode)))

(provide 'xmltop)
;;; xmltop.el ends here
