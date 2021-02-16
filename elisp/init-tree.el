;;; -*- lexical-binding: t -*-

(defun +treemacs-scale-font-size ()
  (face-remap-add-relative 'default :height 0.8))

(leaf treemacs
  :straight t
  :commands treemacs treemacs-select-window
  :bind (("<f1>" . treemacs-select-window)
         (:treemacs-mode-map
          ("<f1>" . treemacs)))
  :custom
  (treemacs-no-png-images . t)
  (treemacs-width . 40)
  :init
  (add-hook 'treemacs-mode-hook '+treemacs-scale-font-size))

(provide 'init-tree)
