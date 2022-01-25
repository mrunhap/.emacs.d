;;; -*- lexical-binding: t -*-

(eat-package rg :straight t)

(eat-package yasnippet
  :straight t
  :commands yas-minor-mode
  :hook ((prog-mode-hook conf-mode-hook) . yas-minor-mode)
  :init
  (eat-package yasnippet-snippets :straight t)
  :config
  (let ((inhibit-message t))
    (yas-reload-all)))

(eat-package company
  :straight t
  :hook
  ((prog-mode-hook conf-mode-hook eshell-mode-hook) . company-mode)
  :commands company-mode
  :init
  (setq
   company-minimum-prefix-length 3
   company-idle-delay 0.2
   company-begin-commands '(self-insert-command
                            backward-delete-char)
   ;; icons
   company-vscode-icons-mapping nil
   company-text-icons-add-background t
   ;; thanks to r/emacs yyoncho
   company-format-margin-function 'company-text-icons-margin
   ;; tooltip frontend config
   company-tooltip-align-annotations t
   company-tooltip-limit 10
   company-tooltip-width-grow-only t
   company-tooltip-idle-delay 0.4
   company-dabbrev-downcase nil
   company-abort-manual-when-too-short t
   company-require-match nil
   company-global-modes '(not dired-mode dired-sidebar-mode)
   company-backends '((company-capf :with company-yasnippet)
                      (company-dabbrev-code company-keywords company-files)
                      company-dabbrev)
   company-files-exclusions '(".git/" ".DS_Store")
   company-tooltip-margin 0)
  :config
  (defun +complete ()
    (interactive)
    (or (yas/expand)
        (company-complete-selection)))
  (define-key company-mode-map [tab] '+complete)
  (define-key company-mode-map (kbd "TAB") '+complete)
  (define-key company-active-map [tab] '+complete)
  (define-key company-active-map (kbd "TAB") '+complete)
  (define-key company-active-map [return] nil)
  (define-key company-active-map (kbd "RET") nil)
  ;; TODO C-e to complete preview
  ;; TODO `company-transformers'
  ;; TODO remove dup cad, like func from keywords and snippets
  )

(provide 'init-completion)
