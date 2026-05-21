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

(defconst silq-keyword-regexp
  (regexp-opt silq-keywords)
  "Regexp matching Silq keyword text without boundary checks.")

(defun silq--identifier-char-p (ch)
  "Return non-nil if CH is part of a Silq identifier."
  (and ch
       (or (eq (char-syntax ch) ?w)
           (eq ch ?_)
           (eq ch ?'))))

(defun silq--identifier-char-at-p (pos)
  "Return non-nil if character at POS is part of a Silq identifier."
  (and (<= (point-min) pos)
       (< pos (point-max))
       (silq--identifier-char-p (char-after pos))))

(defun silq--keyword-matcher (limit)
  "Search for a Silq keyword before LIMIT.
The match itself is exactly the keyword; surrounding delimiter characters are
checked but not consumed."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward silq-keyword-regexp limit t))
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (unless (or (silq--identifier-char-at-p (1- beg))
                    (silq--identifier-char-at-p end))
          (setq found t))))
    found))

(defconst silq-font-lock-keywords
  '((silq--keyword-matcher . font-lock-keyword-face))
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
  (save-excursion
    (let (found)
      (while (and (not found)
                  (re-search-backward "\\_<if\\_>" nil t))
        (unless (or (nth 3 (syntax-ppss))
                    (nth 4 (syntax-ppss)))
          (setq found
                (save-excursion
                  (let ((if-pos (match-beginning 0)))
                    (back-to-indentation)
                    (let* ((indent-pos (point))
                           (spec (silq--current-line-indent-spec))
                           (base (car spec))
                           (existing-align (cdr spec)))
                      (if (re-search-forward "\\_<else\\_>\\s-+" if-pos t)
                          spec
                        (cons base
                              (+ existing-align
                                 (- if-pos indent-pos))))))))))
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
  (save-excursion
    (let ((depth 0)
          found)
      (while (and (not found)
                  (silq--previous-significant-line))
        (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
          (cond
           ((silq--line-nested-if-anchor)
            (setq depth (1+ depth)))

           ((or (silq--line-starts-with-then-p)
                (silq--line-starts-with-else-p))
            (if (> depth 0)
                (setq depth (1- depth))))

           ((save-excursion (silq--first-if-anchor-on-line))
            (if (= depth 0)
                (setq found (save-excursion (silq--first-if-anchor-on-line)))
              (setq depth (1- depth))))

           (t
            (message "no-match  [depth %d]: %s | first-if-anchor: %s"
                     depth line
                     (silq--first-if-anchor-on-line))))))
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
      (or (silq--matching-if-anchor)
          (silq--previous-line-indent-spec)
          0))

     ((silq--line-starts-with-else-p)
      (let ((prev-is-then-if (save-excursion
                               (silq--previous-significant-line)
                               (and (silq--line-starts-with-then-p)
                                    (silq--line-nested-if-anchor)))))
        (if prev-is-then-if
            (or (silq--matching-if-anchor)
                (silq--previous-line-indent-spec)
                0)
          (or (silq--matching-cascade-anchor)
              (silq--matching-if-anchor)
              (silq--previous-line-indent-spec)
              0))))

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
