;;; -*- lexical-binding: t -*-

;; Disable readline based native completion
(setq python-shell-completion-native-enable nil
      python-indent-guess-indent-offset nil)

;; venv
(install-package 'pet)
(add-hook 'python-base-mode-hook 'pet-mode -10)

;; Two ways to make pyright work with installed package.
;;
;; 1. Use venv.
;; pyright need to know venvPath so that it can find the python packages
;; or raise error like "import can't be resolved"
;;
;; 2. Use pdm.
;; Packages installed with pdm under __pypackages__/<version>/lib/,
;; update pyproject.toml to make pyright work with it, for example:
;; [tool.pyright]
;; extraPaths = ["__pypackages__/3.8/lib/", "src/]
;; https://pdm-project.org/en/latest/usage/pep582/#emacs
;;
;; (also check basedpyright and delance)
(defun pyrightconfig-write ()
  "Write a `pyrightconfig.json' file at the root of a project with
`venvPath` and `venv`."
  (interactive)
  (call-interactively #'pyvenv-activate)
  (let* ((json-encoding-pretty-print t)
         (fn (tramp-file-local-name python-shell-virtualenv-root))
         (venvPath (string-trim-right fn "/"))
         (out-file (expand-file-name "pyrightconfig.json" (project-root (project-current)))))
    (with-temp-file out-file
      (insert (json-encode (list :venvPath venvPath
                                 :venv ".venv"))))
    (message "Configured `%s` to use environment `%s`" out-file pyvenv-virtual-env)))


;; ruff, lint and format python code(use apheleia to do format in emacs
(install-package 'flymake-ruff)

(defun my/flymake-ruff-maybe-enable ()
  (when (executable-find "ruff")
    (flymake-ruff-load)))
(add-hook 'python-base-mode-hook 'my/flymake-ruff-maybe-enable)

;; jupyter
;;
;; Better with jupytext and pandoc installed.
(install-package 'code-cells)
(add-hook 'python-base-mode-hook 'code-cells-mode-maybe)

;;; init-python.el ends here
