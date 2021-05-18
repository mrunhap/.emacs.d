;;; -*- lexical-binding: t -*-

(straight-use-package 'elpy)
(straight-use-package 'blacken)
(straight-use-package 'live-py-mode)

;;; elpy
;; pip install pylint
;; pip install flake8 ?
(setq
 elpy-rpc-python-command "python3")

(advice-add 'python-mode :before 'elpy-enable)

;;; blacken
(setq
 blacken-skip-string-normalization t)

(with-eval-after-load "python"
  ;;; live-py-mode
  ;; pip install twisted ?
  (define-key python-mode-map (kbd "C-c l") 'live-py-mode)
  ;;; blacken - reformat python buffer
  ;; format evere time you save
  ;; pip install black
  (add-hook 'python-mode-hook 'blacken-mode))

(setq
 python-indent-offset 4
 python-shell-completion-native-enable nil
 python-shell-interpreter "python3"
 python-indent-guess-indent-offset nil)

(provide 'init-python)
