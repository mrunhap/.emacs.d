;;; -*- lexical-binding: t -*-

(eat-package tree-sitter
  :straight t
  :init
  (eat-package tree-sitter-langs :straight t)
  :hook
  ((go-mode-hook) . tree-sitter-mode)
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :config
  (require 'tree-sitter-langs))

(eat-package clue
  :straight (clue :type git :host github :repo "AmaiKinono/clue"))

(eat-package citre
  :straight (citre :type git :host github :repo "universal-ctags/citre")
  :init
  (require 'citre-config)
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c k") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-peek)
  (global-set-key (kbd "C-x c P") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  (setq citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-auto-enable-citre-mode '(prog-mode))
  (eat-package citre-global
    :init
    (global-set-key (kbd "C-x c r") 'citre-jump-to-reference)
    (global-set-key (kbd "C-x c R") 'citre-ace-peek-references)
    (global-set-key (kbd "C-x c U") 'citre-global-update-database)
    (with-eval-after-load 'citre-peek
      (define-key citre-peek-keymap (kbd "M-l r") 'citre-peek-through-references))
    :config
    (with-eval-after-load "exec-path-from-shell"
      (exec-path-from-shell-copy-envs '("GTAGSOBJDIRPREFIX" "GTAGSCONF" "GTAGSLABEL"))))
  :config
  (define-key citre-peek-keymap (kbd "M-l s") #'citre-peek-save-session)
  (define-key citre-peek-keymap (kbd "M-l h") #'citre-peek-chain-backward)
  (define-key citre-peek-keymap (kbd "M-l l") #'citre-peek-chain-forward)
  (define-key citre-peek-keymap (kbd "M-l j") #'citre-peek-prev-branch)
  (define-key citre-peek-keymap (kbd "M-l k") #'citre-peek-next-branch)
  (define-key citre-peek-keymap (kbd "M-l J") #'citre-peek-move-current-def-up)
  (define-key citre-peek-keymap (kbd "M-l K") #'citre-peek-move-current-def-down))

(eat-package flymake
  :commands flymake-mode
  :after eglot
  :init
  :config
  (define-key flymake-mode-map (kbd "C-c C-b") 'flymake-show-diagnostics-buffer)
  (define-key flymake-mode-map (kbd "M-[") 'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "M-]") 'flymake-goto-next-error))

(eat-package flycheck
  :straight t
  ;; :hook (prog-mode-hook . flycheck-mode)
  :init
  (setq flycheck-temp-prefix ".flycheck"
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-indication-mode (if (display-graphic-p)
                                     'left-fringe
                                   'left-margin))
  :config
  (define-key flycheck-mode-map (kbd "C-c C-b") 'flycheck-list-errors)
  (define-key flycheck-mode-map (kbd "M-[") 'flycheck-previous-error)
  (define-key flycheck-mode-map (kbd "M-]") 'flycheck-next-error))

(eat-package aggressive-indent
  :straight t
  :commands aggressive-indent-mode
  :hook ((emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          scheme-mode-hook
          lisp-mode-hook)
         . aggressive-indent-mode))

(eat-package eglot
  :straight t
  :commands
  eglot-ensure
  :hook (go-mode-hook . eglot-ensure)
  :init
  (setq eglot-stay-out-of nil
        eglot-ignored-server-capabilites '(:documentHighlightProvider))
  ;; I will manage `company-capf' myself
  (add-to-list 'eglot-stay-out-of 'company)
  ;; auto expand function param for golang
  ;; (setq-default eglot-workspace-configuration
  ;;               '((gopls
  ;;                  (usePlaceholders . t))))
  :config
  (define-key eglot-mode-map (kbd "M-RET") 'eglot-code-actions)
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

(require 'init-go)
(require 'init-python)
(require 'init-c)
(require 'init-web)

(provide 'init-dev)
