;;; etd.el --- Examples to Tests and Docs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jason M23
;;
;; Author: Jason M23 <jasonm23@gmail.com>
;;
;; Created: August 14, 2022
;; Modified: August 14, 2022
;; Version: 1.4.5
;; Keywords: lisp tools extensions
;;
;; Homepage: https://github.com/emacsfodder/kurecolor
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Derived from the test/docs functions/macros in magnars/s.el library.
;;
;; ETD allows the simple writing of examples as tests and generation of
;; markdown documents for both etd--functions and examples.
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

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24.1 or later"))

(eval-when-compile
 (if (= 24 emacs-major-version)
     (progn
      (require 'cl)
      (when (locate-library "cl-lib")
        (require 'cl-lib)))
   (require 'cl-lib)))

(defvar etd--testing t "When set to t run tests, when set to nil generate documents.")
(defvar etd--functions '() "Collected functions.")
(defvar etd--float-precision 0.0001)

(defun etd--zip (x y)
  "Zip lists X & Y together as a list of cons cells."
  (when (and (etd--listsp x y)
             (etd--length= x y))
    (let (result
          (c 0)
          (l (length x)))
      (while (< c l)
        (push (cons (nth c x) (nth c y)) result)
        (setq c (1+ c)))
      (reverse result))))

(defun etd--listsp (&rest lists)
  "Return non-nil if all LISTS satisfy `listp'."
  (cl-reduce
   (lambda (c l) (and c (listp l)))
   lists))

(defun etd--compare-flat-lists (x y fn)
  "Compare to flat lists X & Y using FN."
  (cl-reduce
   (lambda (c l) (and c (funcall fn (car l) (cdr l))))
   (etd--zip x y)))

(defun etd--length= (x y)
  "Test (length= X Y)."
  (= (length x) (length y)))

(defun etd--approximate-equal (x y)
  "Test approximate equality of X and Y."
  (if (and (listp x) (etd--listsp x y))
      (etd--lists-approx-equal x y)
    (etd--approx-equal x y)))

(defun etd--approx-equal (x y)
  "Test approximate equality.

In `defexamples' use the form  `X ~> Y'.

Floating point correspondents will be approximated by
`etd--float-precision'"
  (or (= x y)
      (equal x y)
      (and (floatp x) (floatp y)
           (< (/ (abs (- x y))
                 (max (abs x) (abs y)))
              etd--float-precision))))

(defun etd--lists-approx-equal (x y)
  "Test approximate equality of lists of numbers.

In `defexamples' use the from  `X ~> Y'.

Floating point correspondents will be approximated by
`etd--float-precision'"
   (and (etd--listsp x y)
    (and (etd--length= x y))
    (and (etd--compare-flat-lists x y 'etd--approx-equal))))

(defun etd--example-to-test (cmd idx example)
  "Create one `ert-deftest' from CMD, IDX and EXAMPLE."
  (let ((test-name (intern (format "%s-%02i" cmd idx)))
        (actual (car example))
        (operator (nth 1 example))
        (expected (nth 2 example)))
    (cond
      ((string= "=>" operator)
       `(ert-deftest ,test-name ()
           (should (equal-including-properties ,actual ,expected))))

      ((string= "~>" operator)
       `(ert-deftest ,test-name ()
           (should (etd--approximate-equal ,actual ,expected)))))))

(defun etd--examples-to-tests (cmd examples)
  "Create `ert-deftest' for CMD and each of the EXAMPLES."
  (let (result (idx 0))
    (while examples
      (setq idx (1+ idx))
      (setq result (cons (etd--example-to-test cmd idx examples) result))
      (setq examples (cdr (cddr examples))))
    (nreverse result)))

(defmacro defexamples (cmd &rest examples)
  "CMD and EXAMPLES to ert-deftests."
  (declare (indent 1))
  (if etd--testing

   `(progn
     ,@(etd--examples-to-tests cmd examples))

   `(add-to-list 'etd--functions (list
                                  ',cmd
                                  (etd--docs--signature (symbol-function ',cmd))
                                  (etd--docs--docstring (symbol-function ',cmd))
                                  (etd--examples-to-strings ',examples)))))

(defmacro def-example-group (group &rest examples)
  "GROUP of EXAMPLES for docs."
  (declare (indent 1))
  (if etd--testing
      `(progn ,@examples)
   `(progn
      (add-to-list 'etd--functions ,group)
      ,@examples)))

(defun etd--example-to-string (example)
  "EXAMPLE to string."
  (let ((actual (car example))
        (expected (nth 2 example)))
    (cl-reduce
     (lambda (str regexp)
       (replace-regexp-in-string
        (car regexp) (cdr regexp)
        str t t))
     '(("\r" . "\\r")
       ("\t" . "\\t")
       ("\\\\\\?" . "?"))
     :initial-value (format "%S\n ⇒ %S" actual expected))))

(defun etd--examples-to-strings (examples)
  "Create strings from list of EXAMPLES."
  (let (result)
    (while examples
      (setq result (cons (etd--example-to-string examples) result))
      (setq examples (cdr (cddr examples))))
    (nreverse result)))

(defun etd--docs--signature (cmd)
  "Get function signature for CMD."
  (if (eq 'macro (car cmd))
      (nth 2 cmd)
    (cadr cmd)))

(defun etd--docs--docstring (cmd)
  "Get docstring for CMD."
  (if (eq 'macro (car cmd))
      (nth 3 cmd)
    (nth 2 cmd)))

(defun etd--quote-and-downcase (string)
  "Wrap STRING in backquotes for markdown."
  (format "`%s`" (downcase string)))

(defun etd--quote-docstring (docstring)
  "Quote DOCSTRING."
  (if (null docstring)
      ""
    (let ((case-fold-search nil))
     (replace-regexp-in-string
       "\\\\="
       ""
       (replace-regexp-in-string
         "\\([a-z]+\\)`\\([a-z]+\\)"
         "\\1'\\2"
         (replace-regexp-in-string
           "`\\(.*?\\)'"
           "`\\1`"
           (replace-regexp-in-string
            "\\b\\([A-Z][A-Z0-9-]*\\)\\b"
            'etd--quote-and-downcase
            docstring t)))))))

(defvar etd-function-to-md-template
  "### %s %s

%s

```lisp
%s
```
")

(defun etd--function-to-md (function)
  "FUNCTION to markdown."
  (if (stringp function)
      ""
    (let ((command-name (car function))
          (signature (cadr function))
          (docstring (etd--quote-docstring (nth 2 function)))
          (examples (nth 3 function)))
      (format etd-function-to-md-template
              command-name
              (if signature (format "`%s`" signature) "")
              docstring
              (mapconcat 'identity (etd--first-three examples) "\n")))))

(defun etd--docs--chop-suffix (suffix s)
  "Remove SUFFIX from S."
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun etd--github-id (command-name signature)
  "Generate GitHub ID for COMMAND-NAME and SIGNATURE."
  (etd--docs--chop-suffix
   "-"
   (replace-regexp-in-string "[^a-zA-Z0-9-]+" "-" (format "%S %S" command-name (if signature signature "")))))

(defun etd--function-summary (function)
  "Create a markdown summary of FUNCTION."
  (if (stringp function)
      (concat "\n### " function "\n")
    (let ((command-name (car function))
          (signature (cadr function)))
      (format "* [%s](#%s) %s"
              command-name
              (etd--github-id command-name signature)
              (if signature (format "`%s`" signature) "")))))

(defun etd--first-three (list)
  "Select first 3 examples from LIST."
  (let (first)
    (when (car list)
      (setq first (cons (car list) first))
      (when (cadr list)
        (setq first (cons (cadr list) first))
        (when (nth 2 list)
          (setq first (cons (nth 2 list) first)))))
    (nreverse first)))

(defun etd--simplify-quotes ()
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

(defun etd--goto-and-remove (s)
  "Goto S in buffer and remove it."
  (goto-char (point-min))
  (search-forward s)
  (delete-char (- (length s))))

(defun etd--create-docs-file (template readme)
  "Create README from TEMPLATE."
  (interactive "fSelect Template: \nFSelect README.md file: ")
  (let ((etd--functions (nreverse etd--functions)))
    (with-temp-file readme
     (insert-file-contents-literally template)
     (etd--goto-and-remove "[[ function-list ]]")
     (insert (mapconcat 'etd--function-summary etd--functions "\n"))
     (etd--goto-and-remove "[[ function-docs ]]")
     (insert (mapconcat 'etd--function-to-md etd--functions "\n"))
     (etd--simplify-quotes))))

(defun etd-create-docs-file-for-buffer (template readme)
  "Create README from TEMPLATE."
  (interactive "fSelect Template: \nFSelect README.md file: ")
  (setq etd--testing nil)
  (setq etd--functions '())
  (eval-buffer)
  (etd--create-docs-file template readme)
  (setq etd--testing t)
  (eval-buffer))

(defun etd-create-docs-file-for (examples-file template readme)
  "Using EXAMPLES-FILE and TEMPLATE create README."
  (interactive "fSelect Examples file: \nfSelect Template: \nFSelect README.md file: ")
  (setq etd--testing nil)
  (setq etd--functions '())
  (load-file examples-file)
  (etd--create-docs-file template readme))

(provide 'etd)
;;; etd.el ends here
