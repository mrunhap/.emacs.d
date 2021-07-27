;;; -*- lexical-binding: t -*-

(eat-package rime
  :straight t
  :commands toggle-input-method
  :init
  (defun +rime-predicate-org-syntax-punc-p ()
    (when (eq major-mode 'org-mode)
      (member rime--current-input-key '(91 93 42 126))))

  (defun +rime-predicate-md-syntax-punc-p ()
    (when (eq major-mode 'markdown-mode)
      (member rime--current-input-key '(91 93 96))))

  (setq rime-disable-predicates '(meow-normal-mode-p
                                  meow-motion-mode-p
                                  meow-keypad-mode-p
                                  +rime-predicate-org-syntax-punc-p
                                  +rime-predicate-md-syntax-punc-p)
        rime-inline-predicates '(rime-predicate-space-after-cc-p
                                 rime-predicate-current-uppercase-letter-p
                                 +rime-predicate-md-syntax-punc-p)
        rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v")
        rime-inline-ascii-holder ?a
        default-input-method "rime"
        rime-cursor "|"
        rime-show-candidate 'minibuffer
        rime-title "ㄓ ")
  (when sys/macp
    (setq rime-librime-root (expand-file-name "librime/dist" user-emacs-directory)))
  :config
  (define-key rime-active-mode-map [tab] 'rime-inline-ascii)
  (define-key rime-mode-map (kbd "C-`") 'rime-send-keybinding)
  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable))

(provide 'init-rime)
