;;; session-type-mode.el --- major mode for session-types -*- lexical-binding: t; -*-


;;;; set syntax table
(defvar session-type-mode-syntax-table nil "syntax table for session-type-mode")
(setq session-type-mode-syntax-table
      (let ((st (make-syntax-table)))
        ;; comments
        (modify-syntax-entry ?\/ ". 124b" st)
        (modify-syntax-entry ?\n "> b" st)
        (modify-syntax-entry ?* ". 23" st)

        ;; strings
        (modify-syntax-entry ?\" "\"" st)
        (modify-syntax-entry ?< "." st)
        (modify-syntax-entry ?> "." st)

        ;; symbols
        (modify-syntax-entry ?\? "_" st)
        (modify-syntax-entry ?! "_" st)
        (modify-syntax-entry ?| "_" st)
        (modify-syntax-entry ?_ "w" st)
        st))


;;;; font lock for syntax
(setq session-type-basic '("\?" "!" "|" "|-"))
(setq session-type-declare '("type" "proc" "yield" "equal" "define" "end"))
(setq session-type-builtin '("match" "with" "flip"))
(setq session-type-constant '("recv" "send" "work" "get" "pay" "close" "wait" "assume" "assert" "absurd"))
(setq session-type-quantifier '("forall" "exists"))
(setq session-type-modal '("|>" "<|"))
(setq session-type-highlights '("<-" "->" "<->" "=>"))
(setq session-type-intense '("magic") )

(setq session-type-basic-regexp (regexp-opt session-type-basic 'symbols))
(setq session-type-declare-regexp (regexp-opt session-type-declare 'symbols))
(setq session-type-builtin-regexp (regexp-opt session-type-builtin 'symbols))
(setq session-type-constant-regexp (regexp-opt session-type-constant 'symbols))
(setq session-type-quantifier-regexp (regexp-opt session-type-quantifier 'symbols))
(setq session-type-modal-regexp (regexp-opt session-type-modal nil))
(setq session-type-highlights-regexp (regexp-opt session-type-highlights nil))
(setq session-type-intense-regexp (regexp-opt session-type-intense nil))

(setq session-type-font-lock-keywords
      `((,session-type-declare-regexp . font-lock-keyword-face)
        (,session-type-intense-regexp . font-lock-warning-face)
        (,session-type-highlights-regexp . font-lock-type-face)
        (,session-type-modal-regexp . font-lock-constant-face)
        (,session-type-basic-regexp . font-lock-keyword-face)
        (,session-type-builtin-regexp . font-lock-keyword-face)
        (,session-type-constant-regexp . font-lock-constant-face)
        (,session-type-quantifier-regexp . font-lock-constant-face)
        ))


;;;###autoload
(define-derived-mode session-type-mode prog-mode
  "Session-Types"
  "Major mode for editing session types"
  (setq font-lock-defaults '((session-type-font-lock-keywords)))
  (set-syntax-table session-type-mode-syntax-table)
  (setq-local comment-start "//")
  (setq-local comment-start-skip "//+[\t ]*")
  (setq-local comment-end "")
  (setq-local prettify-symbols-alist
              '(("|-" . ?⊢)
                ("|>". ?▹)
                ("<|". ?◃)
                ("->" . ?→)
                ("<-" . ?←)
                ("<->" . ?↔)
                ("=>" . ?⇒)
                ("/\\" . ?∧)
                ("\\/" . ?∨)
                ("<=" . ?≤)
                (">=" . ?≥)
                ("not" . ?¬)
                ("exists" . ?∃)
                ("forall" . ?∀))))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.st" . session-type-mode))
(provide 'session-type-mode)
