;;; silq-mode.el --- Major mode for Silq -*- lexical-binding: t; -*-

(defgroup silq nil
  "Major mode for the Silq language."
  :group 'languages)

(defcustom silq-indent-offset 4
  "Basic indentation width for `silq-mode'."
  :type 'integer
  :group 'silq)

(defconst silq-keywords
  '("as"
    "assert"
    "coerce"
    "const"
    "dat"
    "def"
    "do"
    "else"
    "false"
    "for"
    "forget"
    "if"
    "import"
    "in"
    "lambda"
    "let"
    "lifted"
    "mfree"
    "moved"
    "once"
    "Pi"
    "pun"
    "qfree"
    "quantum"
    "repeat"
    "return"
    "spent"
    "then"
    "true"
    "typeof"
    "while"
    "wild"
    "with")
  "Silq keywords.")

(defconst silq-font-lock-keywords
  (list
   `(,(concat "\\_<" (regexp-opt silq-keywords t) "\\_>")
     . font-lock-keyword-face))
  "Font-lock rules for `silq-mode'.")

(defvar silq-mode-syntax-table
  (let ((st (make-syntax-table prog-mode-syntax-table)))
    ;; Line comments: // ...
    ;; Block comments: /* ... */  (non-nesting)
    ;; Nested block comments: /+ ... +/
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?+ ". 23n" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; Treat underscore as symbol constituent.
    (modify-syntax-entry ?_ "_" st)

    ;; Leave apostrophe alone in the base syntax table.
    ;; We make it a symbol constituent only contextually via
    ;; `syntax-propertize-function', when it trails an identifier.
    st)
  "Syntax table for `silq-mode'.")

(defun silq-syntax-propertize (start end)
  "Make apostrophes after identifiers part of the identifier."
  (goto-char start)
  (funcall
   (syntax-propertize-rules
    ("\\(\\sw\\|\\s_\\)\\('\\{1,\\}\\)"
     (2 (string-to-syntax "_"))))
   start end))

(defun silq--ppss-bol ()
  (save-excursion
    (back-to-indentation)
    (syntax-ppss)))

(defun silq--inside-string-p ()
  (nth 3 (silq--ppss-bol)))

(defun silq--inside-comment-p ()
  (nth 4 (silq--ppss-bol)))

(defun silq--line-starts-with-closing-brace-p ()
  (save-excursion
    (back-to-indentation)
    (eq (char-after) ?})))

(defun silq--line-starts-with-closing-paren-p ()
  (save-excursion
    (back-to-indentation)
    (memq (char-after) '(?\) ?\]))))

(defun silq--line-starts-with-if-p ()
  (save-excursion
    (back-to-indentation)
    (looking-at-p "\\_<if\\_>")))

(defun silq--line-starts-with-then-p ()
  (save-excursion
    (back-to-indentation)
    (looking-at-p "\\_<then\\_>")))

(defun silq--line-starts-with-else-p ()
  (save-excursion
    (back-to-indentation)
    (looking-at-p "\\_<else\\_>")))

(defun silq--previous-significant-line ()
  "Move to previous nonblank, non-comment-only line.
Return non-nil on success."
  (let (found)
    (while (and (not found) (= (forward-line -1) 0))
      (beginning-of-line)
      (unless (looking-at-p "[ \t]*\\(?:$\\|//\\|/\\*\\|/\\+\\)")
        (setq found t)))
    found))

(defun silq--previous-line-indentation ()
  (save-excursion
    (when (silq--previous-significant-line)
      (current-indentation))))

(defun silq--previous-line-ends-with-assign-p ()
  (save-excursion
    (when (silq--previous-significant-line)
      (end-of-line)
      (skip-chars-backward " \t")
      (or (looking-back ":=" (line-beginning-position))
          (looking-back "=" (line-beginning-position))))))

(defun silq--matching-if-column ()
  "Return the column of the nearest preceding `if' token."
  (save-excursion
    (let (found)
      (while (and (not found)
                  (re-search-backward "\\_<if\\_>" nil t))
        (unless (or (nth 3 (syntax-ppss))
                    (nth 4 (syntax-ppss)))
          (goto-char (match-beginning 0))
          (setq found (current-column))))
      found)))

(defun silq--matching-open-delim-indentation ()
  "Return indentation of the line containing the matching opener."
  (let ((open (ignore-errors (scan-lists (point) -1 1))))
    (when open
      (save-excursion
        (goto-char open)
        (current-indentation)))))

(defun silq--brace-indentation-base ()
  "Indent one level inside the containing brace block."
  (let* ((ppss (silq--ppss-bol))
         (open (nth 1 ppss)))
    (when open
      (save-excursion
        (goto-char open)
        (when (eq (char-after) ?{)
          (+ (current-indentation) silq-indent-offset))))))

(defun silq--paren-indentation-base ()
  "Indent one level inside the containing ( or [ form.
This does not align to the opener column; it adds one indentation level."
  (let* ((ppss (silq--ppss-bol))
         (open (nth 1 ppss)))
    (when open
      (save-excursion
        (goto-char open)
        (when (memq (char-after) '(?\( ?\[))
          (+ (current-indentation) silq-indent-offset))))))

(defun silq-calculate-indentation ()
  "Compute indentation for current line."
  (save-excursion
    (beginning-of-line)
    (cond
     ((bobp)
      0)

     ((silq--inside-string-p)
      (current-indentation))

     ((silq--inside-comment-p)
      (current-indentation))

     ;; then / else align with the actual `if' token
     ((silq--line-starts-with-then-p)
      (or (silq--matching-if-column)
          (silq--previous-line-indentation)
          0))

     ((silq--line-starts-with-else-p)
      (or (silq--matching-if-column)
          (silq--previous-line-indentation)
          0))

     ;; closing delimiters align with the line containing their opener
     ((silq--line-starts-with-closing-brace-p)
      (or (silq--matching-open-delim-indentation)
          0))

     ((silq--line-starts-with-closing-paren-p)
      (or (silq--matching-open-delim-indentation)
          0))

     ;; if after := gets one extra level
     ((silq--line-starts-with-if-p)
      (if (silq--previous-line-ends-with-assign-p)
          (+ (or (silq--previous-line-indentation) 0)
             silq-indent-offset)
        (or (silq--brace-indentation-base)
            (silq--paren-indentation-base)
            0)))

     ;; inside (...) or [...] => one extra level, not opener-column alignment
     ((silq--paren-indentation-base)
      (silq--paren-indentation-base))

     ;; inside {...}
     (t
      (or (silq--brace-indentation-base)
          0)))))

(defun silq-indent-line ()
  "Indent current line according to Silq structure."
  (interactive)
  (let ((target (silq-calculate-indentation))
        (offset (- (current-column) (current-indentation))))
    (indent-line-to target)
    (when (> offset 0)
      (move-to-column (+ target offset)))))

(define-derived-mode silq-mode prog-mode "Silq"
  "Major mode for editing Silq."
  :syntax-table silq-mode-syntax-table
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t)
  (setq-local font-lock-defaults '(silq-font-lock-keywords))
  (setq-local syntax-propertize-function #'silq-syntax-propertize)
  (setq-local indent-line-function #'silq-indent-line)
  (setq-local electric-indent-chars
              (append "{}[]();"
                      electric-indent-chars))
  (setq-local comment-start "/+ ")
  (setq-local comment-end " +/")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\|/\\+\\)\\s *")
  (setq-local compile-command
              (concat "time silq "
                      (shell-quote-argument
                       (file-relative-name (or buffer-file-name "")))
                      " "))
  (syntax-propertize (point-max)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slq\\'" . silq-mode))

(provide 'silq-mode)

;;; silq-mode.el ends here
