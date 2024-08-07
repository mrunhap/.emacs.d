;;; -*- lexical-binding: t -*-

(defun font-installed-p (font-list)
  (catch 'font-found
    (dolist (font font-list)
      (when (find-font (font-spec :name font))
        (throw 'font-found font)))))

(defun my/setup-font ()
  (let* ((my/font-default        (font-installed-p my/fonts-default))
         (my/font-variable-pitch (font-installed-p my/fonts-variable-pitch))
         (my/font-cjk            (font-installed-p my/fonts-cjk))
         (my/font-unicode        (font-installed-p my/fonts-unicode))
         (my/font-emoji          (font-installed-p my/fonts-emoji))
         (my/font-rescale-alist  `((,my/font-cjk     . 0.95)
                                   (,my/font-emoji   . 0.9)
                                   (,my/font-unicode . 0.95)
                                   (,my/font-variable-pitch . 1.2))))
    (set-face-attribute 'default nil :height (* 10 my/font-size-default))
    (when my/font-default
      (set-face-attribute 'default     nil :family my/font-default)
      (set-face-attribute 'fixed-pitch nil :font my/font-default))
    (when my/font-variable-pitch
      (set-face-font 'variable-pitch my/font-variable-pitch))
    (when my/font-unicode
      (set-fontset-font t 'unicode my/font-unicode))
    (when my/font-emoji
      (set-fontset-font t 'emoji   my/font-emoji))
    (when my/font-cjk
      (set-fontset-font t 'kana     my/font-cjk)
      (set-fontset-font t 'han      my/font-cjk)
      (set-fontset-font t 'cjk-misc my/font-cjk))
    (dolist (setting my/font-rescale-alist)
      (when (car setting)
        (setf (alist-get (car setting)
                         face-font-rescale-alist nil nil #'equal)
              (cdr setting))))))



(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'my/setup-font)
  (add-hook 'after-init-hook #'my/setup-font))

(defun my/fixed-pitch-setup ()
  (interactive)
  (setq buffer-face-mode-face '(:family "Sarasa Mono SC"))
  (buffer-face-mode +1))

;;; init-font.el ends here
