(benchmark-run 1000 (format-mode-line mode-line-format))

(defun +smart-file-name-cached ()
  (if (eq (buffer-name) (car +smart-file-name-cache))
      (cdr +smart-file-name-cache)
    (let ((file-name (+smart-file-name)))
      (setq +smart-file-name-cache
            (cons (buffer-name) file-name))
      file-name)))

(defvar +smart-file-name-cache nil)

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (let ((vc-dir (vc-root-dir))
        (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn vc-dir)
      (file-relative-name bfn vc-dir))
     (bfn bfn)
     (t (buffer-name)))))

(defun luna-mode-line-with-padding (text)
  "Return TEXT with padding on the left.
The padding pushes TEXT to the right edge of the mode-line."
  (if (display-graphic-p)
      (let* ((len (string-pixel-width text))
             (space-prop
              `(space :align-to (- (+ right right-margin) (,len))))
             (padding (propertize "-" 'display space-prop)))
        (concat padding text))
    (concat " " text)))

(setq-default mode-line-format
              (let* ((spaces
                      (propertize " " 'display '(space :width 1.5)))
                     (fringe (propertize
                              " " 'display '(space :width fringe)))
                     (percentage
                      '(format
                        "[%%l] %d%% "
                        (/ (* (window-end) 100.0) (point-max)))))
                `(,fringe
                  (:eval (when (fboundp 'meow-indicator) (meow-indicator)))
                  ,spaces
                  (:eval (when (fboundp 'rime-lighter) (rime-lighter)))
                  ,spaces
                  (:eval (propertize (+smart-file-name-cached) 'face 'mode-line-buffer-id))
                  ,spaces
                  ,spaces
                  mode-name
                  ,spaces
                  vc-mode
                  ,spaces
                  (:eval (when (bound-and-true-p flycheck-mode) flycheck-mode-line))
                  (:eval (when (bound-and-true-p flymake-mode) flymake-mode-line-format))
                  (:propertize " " display (height 1.4))
                  (:propertize " " display (raise -0.3))
                  (:eval (concat (luna-mode-line-with-padding ,percentage)
                                 "%%"))
                  )))

(eat-package doom-modeline
  :straight t
  :hook (after-init-hook . doom-modeline-mode)
  :init
  (setq doom-modeline-irc nil
        doom-modeline-mu4e nil
        doom-modeline-gnus nil
        doom-modeline-github nil
        doom-modeline-persp-name nil
        doom-modeline-unicode-fallback t
        doom-modeline-enable-work-count nil)
  (setq doom-modeline-project-detection 'project))