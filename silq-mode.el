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

(defun silq--matching-if-anchor ()
  "Return indentation info for the nearest preceding `if'.
The result is a cons cell (BASE . ALIGN), where BASE is the
leading indentation in columns and ALIGN is the number of literal
characters from the end of indentation to the `if' token."
  (save-excursion
    (let (found)
      (while (and (not found)
                  (re-search-backward "\\_<if\\_>" nil t))
        (unless (or (nth 3 (syntax-ppss))
                    (nth 4 (syntax-ppss)))
          (let ((if-pos (match-beginning 0)))
            (goto-char if-pos)
            (let ((col (current-column)))
              (back-to-indentation)
              (setq found
                    (cons (current-column)
                          (- if-pos (point))))))))
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
  "Indent one level inside the containing ( or [ form."
  (let* ((ppss (silq--ppss-bol))
         (open (nth 1 ppss)))
    (when open
      (save-excursion
        (goto-char open)
        (when (memq (char-after) '(?\( ?\[))
          (+ (current-indentation) silq-indent-offset))))))

(defun silq-calculate-indentation ()
  "Compute indentation for current line.

Return either an integer column, or a cons (BASE . ALIGN).
(BASE . ALIGN) means: indent BASE columns structurally, then add
ALIGN literal spaces for alignment."
  (save-excursion
    (beginning-of-line)
    (cond
     ((bobp)
      0)

     ((silq--inside-string-p)
      (current-indentation))

     ((silq--inside-comment-p)
      (current-indentation))

     ;; then / else align with the actual `if' token,
     ;; but using spaces after the line's base indentation.
     ((silq--line-starts-with-then-p)
      (or (silq--matching-if-anchor)
          (silq--previous-line-indentation)
          0))

     ((silq--line-starts-with-else-p)
      (or (silq--matching-if-anchor)
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

     ;; inside (...) or [...] => one extra level
     ((silq--paren-indentation-base)
      (silq--paren-indentation-base))

     ;; inside {...}
     (t
      (or (silq--brace-indentation-base)
          0)))))

(defun silq-indent-line ()
  "Indent current line according to Silq structure.
Use tabs for indentation levels and spaces for alignment."
  (interactive)
  (let* ((spec (silq-calculate-indentation))
         (base (if (consp spec) (car spec) spec))
         (align (if (consp spec) (cdr spec) 0))
         (tabs (/ base silq-indent-offset))
         (spaces (+ (% base silq-indent-offset) align))
         (prefix (concat
                  (make-string tabs ?\t)
                  (make-string spaces ?\s)))
         (pos-from-eol (- (point-max) (point))))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (insert prefix))
    (goto-char (- (point-max) pos-from-eol))
    (when (< (current-column) (+ base align))
      (move-to-column (+ base align)))))

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
