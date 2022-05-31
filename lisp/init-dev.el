;;; -*- lexical-binding: t -*-

(eat-package clue
  :straight (clue :type git :host github :repo "AmaiKinono/clue"))

(eat-package citre
  :straight (citre :type git :host github :repo "universal-ctags/citre")
  :init
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  (global-set-key (kbd "C-x c p") 'citre-peek)
  (setq citre-default-create-tags-file-location 'global-cache
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t
        citre-auto-enable-citre-mode-modes '(prog-mode))
  (defun eat/citre-enable ()
    (interactive)
    (citre-mode 1)
    (require 'citre-config))
  (defun eat/citre-disable ()
    (interactive)
    (citre-mode 0)
    (remove-hook 'find-file-hook #'citre-auto-enable-citre-mode))
  (eat-package citre-global
    :init
    ;; NOTE
    ;; Notice that GTAGSOBJDIRPREFIX must exist for gtags to use it. So you need to run:
    ;; $ mkdir -p ~/.cache/gtags/
    (global-set-key (kbd "C-x c U") 'citre-global-update-database)
    (global-set-key (kbd "C-x c P") 'citre-peek-references)
    :config
    (setenv "GTAGSOBJDIRPREFIX" (concat (getenv "HOME") "/.cache/gtags"))
    (setenv "GTAGSCONF" (concat (getenv "HOME") "/.globalrc"))
    (setenv "GTAGSLABEL" "native-pygments")))

;;; Lint

(eat-package flymake-flycheck
  :straight t
  :init
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-disabled-checkers '(emacs-lisp
                                     emacs-lisp-checkdoc
                                     emacs-lisp-package
                                     go-gofmt
                                     go-golint
                                     go-vet
                                     go-build
                                     go-test
                                     go-errcheck
                                     go-unconvert))

  (defun eat/flymake-flycheck-enable ()
    (interactive)
    (setq-local flymake-diagnostic-functions
                (append flymake-diagnostic-functions
                        (flymake-flycheck-all-chained-diagnostic-functions)))
    (flymake-mode 1)))

;;; Misc

(eat-package aggressive-indent
  :straight t
  :commands aggressive-indent-mode
  :hook ((emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          scheme-mode-hook
          lisp-mode-hook)
         . aggressive-indent-mode))

;; HACK Doesn't support `TRAMP' now
(eat-package apheleia
  :straight t
  :hook (go-mode-hook . apheleia-mode)
  :init
  (setq apheleia-remote-algorithm 'local)
  :config
  (setf (alist-get 'gofmt apheleia-formatters)
        '("goimports")))

(eat-package xscheme)

(eat-package dumb-jump
  :straight t
  :init
  (setq
   dumb-jump-quiet t
   dumb-jump-aggressive t
   dumb-jump-selector 'completing-read))

(eat-package eglot
  :straight t
  :commands eglot-ensure
  :init
  (setq
   eglot-events-buffer-size 0
   eglot-ignored-server-capabilites '(:documentHighlightProvider)

   ;; don't block of LSP connection attempts
   eglot-sync-connect nil

   ;; I will manage these myself
   eglot-stay-out-of '(company flymake)

   ;; make eglot manage file out of project by `xref-find-definitions'
   eglot-extend-to-xref t)

  :config
  ;; Add `eglot-flymake-backend' to `flymake-diagnostic-functions' manually
  ;; So that can run other lints together with `eglot-flymake'
  ;; And make sure that lsp check first run
  (with-eval-after-load 'flymake
    (add-to-list 'flymake-diagnostic-functions 'eglot-flymake-backend))

  ;; keybindings
  (define-key eglot-mode-map (kbd "M-RET") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c h") 'eldoc)
  (define-key eglot-mode-map (kbd "M-'") 'eglot-find-implementation)
  (global-set-key (kbd "<f2>") 'eglot)

  ;; TODO code actions
  (add-to-list 'eglot-server-programs
               '(sql-mode . ("sqls" "-config" "~/.config/sqls/config.yaml")))
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
			   '(rust-mode "rust-analyzer")))

;; TODO auto download ltex-ls into emacs root dir and extract to ltex-ls
(eat-package eglot-ltex
  :straight (eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex")
  :hook
  ;; ((markdown-mode-hook org-mode-hook) . eglot-ltex) NOTE enable manually
  :init
  (defun eglot-ltex ()
    (interactive)
    (setq eglot-languagetool-server-path (expand-file-name "ltex-ls/" user-emacs-directory))
    (require 'eglot-ltex)
    (call-interactively #'eglot)))

;; this need pip install epc
(eat-package lsp-bridge
  :straight (lsp-bridge
             :type git
             :host github
             :repo "manateelazycat/lsp-bridge"
             :files (:defaults "*.py" "core/*" "langserver/*")
             ;; do not generate autoload file
             ;; it has an annoying hook to `post-command-hook'
             ;; and emacs have to load this package or
             ;; it will cause an error on startup
             :build (:not autoloads))
  :commands lsp-bridge-mode
  :init
  (setq lsp-bridge-completion-provider 'corfu)
  :config
  (define-key lsp-bridge-mode-map (kbd "M-.") #'lsp-bridge-find-def)
  (define-key lsp-bridge-mode-map (kbd "M-,") #'lsp-bridge-return-from-def)
  (define-key lsp-bridge-mode-map (kbd "M-?") #'lsp-bridge-find-references)
  (define-key lsp-bridge-mode-map (kbd "M-'") #'lsp-bridge-find-impl)
  (define-key lsp-bridge-mode-map (kbd "C-x 4 .") #'lsp-bridge-find-def-other-window)
  (define-key lsp-bridge-mode-map (kbd "C-c r") #'lsp-bridge-rename))

(require 'init-go)
(require 'init-web)
(require 'init-clojure)

(provide 'init-dev)
