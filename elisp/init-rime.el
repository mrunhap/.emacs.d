;;; -*- lexical-binding: t -*-

(use-package rime
  :commands (toggle-input-method)
  :straight
  (rime :type git
        :host github
        :repo "DogLooksGood/emacs-rime"
        :files ("*.el" "Makefile" "lib.c"))
  :bind
  (:map rime-active-mode-map
        ("<tab>" . 'rime-inline-ascii))
  (:map rime-mode-map
        ("C-`" . 'rime-send-keybinding)
        ("M-j" . 'rime-force-enable))
  :custom
  ;; TODO set rime-share-data-dir and rime-usr-data-dir in macos and linux
  (cond (sys/mac-x-p (rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))))
  (rime-disable-predicates '(meow-normal-mode-p
                             meow-motion-mode-p
                             meow-keypad-mode-p))
  (rime-inline-predicates '(rime-predicate-space-after-cc-p
                            rime-predicate-current-uppercase-letter-p))
  (rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g"))
  (rime-inline-ascii-holder ?a)
  (default-input-method "rime")
  (rime-cursor "|")
  (rime-title "rime")
  (rime-show-candidate 'minibuffer))

(provide 'init-rime)
