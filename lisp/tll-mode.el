;;; tll-mode.el --- major mode for tll -*- lexical-binding: t; -*-


;;;; set syntax table
(defvar tll-mode-syntax-table nil "syntax table for tll-mode")
(setq tll-mode-syntax-table
      (let ((st (make-syntax-table)))
        ;; comments
        (modify-syntax-entry ?/ ". 14nb" st)
        (modify-syntax-entry ?- ". 123" st)
        (modify-syntax-entry ?\n ">" st)

        ;; strings
        (modify-syntax-entry ?\" "\"" st)
        (modify-syntax-entry ?\' "\"" st)

        (modify-syntax-entry ?< "." st)
        (modify-syntax-entry ?> "." st)

        ;; symbols
        (modify-syntax-entry ?# "_" st)
        (modify-syntax-entry ?\? "_" st)
        (modify-syntax-entry ?! "_" st)
        (modify-syntax-entry ?: "_" st)
        (modify-syntax-entry ?_ "w" st)
        (modify-syntax-entry ?• "w" st)
        (modify-syntax-entry ?‹ "(›" st)
        (modify-syntax-entry ?› ")‹" st)
        st))


;;;; font lock for syntax
(setq tll-basic '("#" "\?" "!" ":"))
(setq tll-pragma '("program" "logical" "hint"))
(setq tll-sorts '("U" "L" "Type"))
(setq tll-keywords '("of" "layout"))
(setq tll-lambda '("fn" "ln" "fun" "function" "lam" "val" "main" "where"))
(setq tll-builtin '("let" "let*" "in" "match" "as" "with" "end" "if" "then" "else" "return"))
(setq tll-constant '("fork" "recv" "send" "send" "recv" "close" "end" "•" "lazy" "force"))
(setq tll-quantifier '("∀" "forall" "∃" "exists"))
(setq tll-highlights '("→" "->" ".→" ".->" "⊸" "-o" "≔" ":=" "⇒" "=>" "⇑" "⇓"))
(setq tll-intense '("#magic" "!!") )

(setq tll-basic-regexp (regexp-opt tll-basic 'symbols))
(setq tll-pragma-regexp (regexp-opt tll-pragma 'symbols))
(setq tll-sorts-regexp (regexp-opt tll-sorts 'symbols))
(setq tll-keywords-regexp (regexp-opt tll-keywords 'symbols))
(setq tll-lambda-regexp (regexp-opt tll-lambda 'symbols))
(setq tll-builtin-regexp (regexp-opt tll-builtin 'symbols))
(setq tll-constant-regexp (regexp-opt tll-constant 'symbols))
(setq tll-quantifier-regexp (regexp-opt tll-quantifier 'symbols))
(setq tll-highlights-regexp (regexp-opt tll-highlights nil))
(setq tll-intense-regexp (regexp-opt tll-intense nil))


;;;; font lock for log messages
(setq tll-log-group1 '("infer_tm"))
(setq tll-log-group2 '("check_tm"))
(setq tll-log-group3 '("assert_equal" "simpl_iprbm" "simpl_pprbm"))

(setq tll-log-group1-regexp (regexp-opt tll-log-group1 nil))
(setq tll-log-group2-regexp (regexp-opt tll-log-group2 nil))
(setq tll-log-group3-regexp (regexp-opt tll-log-group3 nil))


(setq tll-font-lock-keywords
      `(;; top level declarations
        ("\\(\\<inductive\\>\\|\\<def\\>\\|\\<lemma\\>\\|\\<theorem\\>\\|\\<tmpl\\>\\|\\<impl\\>\\|\\<extern\\>\\|\\<notation\\>\\)\s*\\(\\w*\\)"
         (1 font-lock-keyword-face)
         (2 font-lock-variable-name-face))
        ;; expression syntax
        (,tll-intense-regexp . font-lock-warning-face)
        (,tll-highlights-regexp . font-lock-type-face)
        (,tll-basic-regexp . font-lock-keyword-face)
        (,tll-pragma-regexp . font-lock-keyword-face)
        (,tll-sorts-regexp . font-lock-constant-face)
        (,tll-keywords-regexp . font-lock-keyword-face)
        (,tll-lambda-regexp . font-lock-keyword-face)
        (,tll-builtin-regexp . font-lock-builtin-face)
        (,tll-constant-regexp . font-lock-constant-face)
        (,tll-quantifier-regexp . font-lock-constant-face)
        ;; log file messages
        (,tll-log-group1-regexp . font-lock-string-face)
        (,tll-log-group2-regexp . font-lock-warning-face)
        (,tll-log-group3-regexp . font-lock-doc-face)
        ))


;;;###autoload
(define-derived-mode tll-mode prog-mode
  "TLL"
  "Major mode for editing TLL"
  (setq font-lock-defaults '((tll-font-lock-keywords)))
  (set-syntax-table tll-mode-syntax-table)
  (setq-local comment-start "--")
  (setq-local comment-start-skip "--+[\t ]*")
  (setq-local comment-end "")
  (setq-local prettify-symbols-alist
              '(("->" . ?→)
                (".->" . (?. (Br . Bl) ?→))
                ("<-" . ?←)
                ("=>" . ?⇒)
                ("-o" . ?⊸)
                (":=" . ?≔)
                ("exists" . ?∃)
                ("forall" . ?∀))))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tll" . tll-mode))
(provide 'tll-mode)
