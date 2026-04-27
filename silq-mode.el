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

    ;; Apostrophe is handled contextually in `silq-syntax-propertize'.
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

(defun silq--current-line-indent-spec ()
  "Return current line indentation as (BASE . ALIGN).

BASE is indentation contributed by complete indentation levels.
ALIGN is extra spaces after that indentation.

This assumes indentation is tabs first, then spaces."
  (save-excursion
    (beginning-of-line)
    (let ((tabs 0)
          (spaces 0))
      (while (eq (char-after) ?\t)
        (setq tabs (1+ tabs))
        (forward-char 1))
      (while (eq (char-after) ?\s)
        (setq spaces (1+ spaces))
        (forward-char 1))
      (cons (* tabs silq-indent-offset) spaces))))

(defun silq--previous-line-indent-spec ()
  (save-excursion
    (when (silq--previous-significant-line)
      (silq--current-line-indent-spec))))

(defun silq--previous-line-ends-with-assign-p ()
  (save-excursion
    (when (silq--previous-significant-line)
      (end-of-line)
      (skip-chars-backward " \t")
      (or (looking-back ":=" (line-beginning-position))
          (looking-back "=" (line-beginning-position))))))

(defun silq--first-if-anchor-on-line ()
  "Return (BASE . ALIGN) for the first `if' token on the current line, or nil."
  (save-excursion
    (let ((eol (line-end-position))
          found)
      (back-to-indentation)
      (while (and (not found)
                  (re-search-forward "\\_<if\\_>" eol t))
        (unless (or (nth 3 (syntax-ppss (match-beginning 0)))
                    (nth 4 (syntax-ppss (match-beginning 0))))
          (let ((if-pos (match-beginning 0)))
            (back-to-indentation)
            (setq found
                  (cons (current-indentation)
                        (- if-pos (point)))))))
      found)))

(defun silq--line-nested-if-anchor ()
  "If current line starts with then/else and contains a nested `if',
return its anchor (BASE . ALIGN)."
  (save-excursion
    (back-to-indentation)
    (when (or (looking-at-p "\\_<then\\_>")
              (looking-at-p "\\_<else\\_>"))
      (silq--first-if-anchor-on-line))))

(defun silq--matching-if-anchor ()
  "Return indentation info for the nearest preceding `if'.

The result is (BASE . ALIGN), where BASE is structural indentation
in columns and ALIGN is extra spaces after that indentation."
  (save-excursion
    (let (found)
      (while (and (not found)
                  (re-search-backward "\\_<if\\_>" nil t))
        (unless (or (nth 3 (syntax-ppss))
                    (nth 4 (syntax-ppss)))
          (let ((if-pos (match-beginning 0)))
            (goto-char if-pos)
            (let ((indent-pos (progn
                                (back-to-indentation)
                                (point))))
              (setq found
                    (cons (save-excursion
                            (goto-char indent-pos)
                            (current-indentation))
                          (- if-pos indent-pos)))))))
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

(defun silq--matching-cascade-anchor ()
  "Return the anchor for the current then/else line.

Scan backward over significant lines and find the corresponding
if / else-if cascade anchor. Nested towers introduced by
`then if ...' or `else if ...' are skipped once both of their
arms have been seen."
  (save-excursion
    (let ((pending-arms 0)
          found)
      (while (and (not found)
                  (silq--previous-significant-line))
        (cond
         ;; A previous then/else line that itself contains an `if`
         ;; starts a nested cascade.
         ;;
         ;; If we have seen fewer than two plain arms since that line,
         ;; then the current line still belongs to that nested cascade.
         ;; Otherwise that nested cascade is complete, so skip it and
         ;; continue searching outward.
         ((silq--line-nested-if-anchor)
          (if (< pending-arms 2)
              (setq found (silq--line-nested-if-anchor))
            (setq pending-arms (- pending-arms 2))))

         ;; A plain then/else arm contributes one arm to the most recent
         ;; cascade we are scanning backward through.
         ((or (silq--line-starts-with-then-p)
              (silq--line-starts-with-else-p))
          (setq pending-arms (1+ pending-arms)))

         ;; Any earlier line containing an `if` can anchor a cascade.
         ;; Same logic: if we have not already consumed both arms of a
         ;; more recent cascade, this is the matching anchor.
         ((silq--first-if-anchor-on-line)
          (if (< pending-arms 2)
              (setq found (silq--first-if-anchor-on-line))
            (setq pending-arms (- pending-arms 2))))))

      found)))

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

     ;; then / else align to the corresponding if / else-if cascade
     ((silq--line-starts-with-then-p)
      (or (silq--matching-cascade-anchor)
          (silq--matching-if-anchor)
          (silq--previous-line-indent-spec)
          0))

     ((silq--line-starts-with-else-p)
      (or (silq--matching-cascade-anchor)
          (silq--matching-if-anchor)
          (silq--previous-line-indent-spec)
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
Use tabs for indentation and spaces for alignment."
  (interactive)
  (let* ((spec (silq-calculate-indentation))
         (base (if (consp spec) (car spec) spec))
         (align (if (consp spec) (cdr spec) 0))
         (target (+ base align))
         (tabs (/ base silq-indent-offset))
         (spaces (+ (% base silq-indent-offset) align))
         (prefix (concat
                  (make-string tabs ?\t)
                  (make-string spaces ?\s)))
         (orig-col (current-column))
         (orig-indent (current-indentation))
         (in-indent (<= (point)
                        (save-excursion
                          (back-to-indentation)
                          (point)))))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (insert prefix))
    (if in-indent
        (move-to-column target)
      (move-to-column (+ target (max 0 (- orig-col orig-indent)))))))

;;;###autoload
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
  (set-input-method "Agda")
  (syntax-propertize (point-max)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.slq\\'" . silq-mode))

(provide 'silq-mode)

;;; silq-mode.el ends here
