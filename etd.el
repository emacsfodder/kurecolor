;;; etd.el --- Examples to Tests and Docs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jason M23
;;
;; Author: Jason M23 <jasonm23@gmail.com>
;; Maintainer: Jason M23 <jasonm23@gmail.com>
;; Created: August 14, 2022
;; Modified: August 14, 2022
;; Version: 1.4.0
;; Keywords: tests examples documentation markdown
;; Homepage: https://github.com/emacsfodder/kurecolor
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Derived from the test/docs functions/macros in magnars/s.el library.
;;
;; ETD allows the simple writing of examples as tests and generation of
;; markdown documents for both functions and examples.
;;
;; Currently this package is dogfooding in emacsfodder/kurecolor and will be
;; backported to magnars/s.el for further testing at a suitable time. It
;; will be released to MELPA shortly after that.
;;
;; ## Usage:
;;
;; The easiest way to understand how to use `etd' is to check out
;; `kurecolor-examples.el' in this folder.
;;
;;; Code:

(require 'ert)

(defvar etd-testing t "When set to t run tests, when set to nil generate documents.")
(defvar functions '() "Collected functions.")

(defun examples-to-should-1 (examples)
  "Create one `should' from EXAMPLES."
  (let ((actual (car examples))
        (expected (caddr examples)))
    `(let ((previous-match-data (match-data)))
       (should (equal-including-properties ,actual ,expected))
       (should (equal (match-data) previous-match-data)))))

(defun examples-to-should (examples)
  "Create `should' for all EXAMPLES."
  (let (result)
    (while examples
      (setq result (cons (examples-to-should-1 examples) result))
      (setq examples (cdddr examples)))
    (nreverse result)))

(defmacro defexamples (cmd &rest examples)
  "CMD and EXAMPLES to ert-deftests."
  (declare (indent 1))
  (if etd-testing

   `(ert-deftest ,cmd ()
      ,@(examples-to-should examples))

   `(add-to-list 'functions (list
                             ',cmd
                             (docs--signature (symbol-function ',cmd))
                             (docs--docstring (symbol-function ',cmd))
                             (examples-to-strings ',examples)))))

(defmacro def-example-group (group &rest examples)
  "GROUP of EXAMPLES for docs."
  (declare (indent 1))
  (if etd-testing
      `(progn ,@examples)
   `(progn
      (add-to-list 'functions ,group)
      ,@examples)))

(defun example-to-string (example)
  "EXAMPLE to string."
  (let ((actual (car example))
        (expected (caddr example)))
    (cl-reduce
     (lambda (string regexp)
       (replace-regexp-in-string
        (car regexp) (cdr regexp)
        string t t))
     '(("\r" . "\\r")
       ("\t" . "\\t")
       ("\\\\\\?" . "?"))
     :initial-value (format "%S\n ⇒ %S" actual expected))))

(defun examples-to-strings (examples)
  "Create strings from list of EXAMPLES."
  (let (result)
    (while examples
      (setq result (cons (example-to-string examples) result))
      (setq examples (cdddr examples)))
    (nreverse result)))

(defun docs--signature (cmd)
  "Get signature for CMD."
  (if (eq 'macro (car cmd))
      (caddr cmd)
    (cadr cmd)))

(defun docs--docstring (cmd)
  "Get docstring for CMD."
  (if (eq 'macro (car cmd))
      (cadddr cmd)
    (caddr cmd)))

(defun quote-and-downcase (string)
  "Wrap STRING in backquotes for markdown."
  (format "`%s`" (downcase string)))

(defun quote-docstring (docstring)
  "Quote DOCSTRING."
  (if (null docstring)
      ""
   (let (case-fold-search)
    (replace-regexp-in-string
     "`\\([^ ]+\\)'"
     "`\\1`"
     (replace-regexp-in-string
      "\\b\\([A-Z][A-Z0-9-]*\\)\\b"
      'quote-and-downcase
      docstring t)))))

(defun function-to-md (function)
  "FUNCTION to markdown."
  (if (stringp function)
      ""
    (let ((command-name (car function))
          (signature (cadr function))
          (docstring (quote-docstring (caddr function)))
          (examples (cadddr function)))
      (format "### %s %s\n\n%s\n\n```lisp\n%s\n```\n"
              command-name
              (if signature (format "`%s`" signature) "")
              docstring
              (mapconcat 'identity (first-three examples) "\n")))))

(defun docs--chop-suffix (suffix s)
  "Remove SUFFIX from S."
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun github-id (command-name signature)
  "Generate GitHub ID for COMMAND-NAME and SIGNATURE."
  (docs--chop-suffix
   "-"
   (replace-regexp-in-string "[^a-zA-Z0-9-]+" "-" (format "%S %S" command-name signature))))

(defun function-summary (function)
  "Create a markdown summary of FUNCTION."
  (if (stringp function)
      (concat "\n### " function "\n")
    (let ((command-name (car function))
          (signature (cadr function)))
      (format "* [%s](#%s) `%s`" command-name (github-id command-name signature) signature))))

(defun simplify-quotes ()
  "Simplify quotes in buffer."
  (goto-char (point-min))
  (while (search-forward "(quote nil)" nil t)
    (replace-match "'()"))
  (goto-char (point-min))
  (while (search-forward "(quote " nil t)
    (forward-char -7)
    (let ((p (point)))
      (forward-sexp 1)
      (delete-char -1)
      (goto-char p)
      (delete-char 7)
      (insert "'"))))

(defun goto-and-remove (s)
  "Goto S in buffer and remove it."
  (goto-char (point-min))
  (search-forward s)
  (delete-char (- (length s))))

(defun create-docs-file (template readme)
  "Create README from TEMPLATE."
  (interactive "fSelect Template: \nFSelect README.md file: ")
  (let ((functions (nreverse functions)))
    (with-temp-file readme
     (insert-file-contents-literally template)
     (goto-and-remove "[[ function-list ]]")
     (insert (mapconcat 'function-summary functions "\n"))
     (goto-and-remove "[[ function-docs ]]")
     (insert (mapconcat 'function-to-md functions "\n"))
     (simplify-quotes))))

(defun create-docs-file-for-buffer (template readme)
  "Create README from TEMPLATE."
  (interactive "fSelect Template: \nFSelect README.md file: ")
  (setq etd-testing nil)
  (setq functions '())
  (eval-buffer)
  (create-docs-file template readme)
  (setq etd-testing t)
  (eval-buffer))

(defun create-docs-file-for (examples-file template readme)
  "Using EXAMPLES-FILE and TEMPLATE create README."
  (interactive "fSelect Examples file: \nfSelect Template: \nFSelect README.md file: ")
  (setq etd-testing nil)
  (setq functions '())
  (load-file examples-file)
  (create-docs-file template readme))

(defun first-three (list)
  "Select first 3 examples from LIST."
  (let (first)
    (when (car list)
      (setq first (cons (car list) first))
      (when (cadr list)
        (setq first (cons (cadr list) first))
        (when (caddr list)
          (setq first (cons (caddr list) first)))))
    (nreverse first)))

(provide 'etd)
;;; etd.el ends here
