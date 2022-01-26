(setq
 ;; Do not initialise the package manager.  This is done in `init.el'.
 package-enable-at-startup nil
 ;; Resizing the Emacs frame can be a terribly expensive part of changing the
 ;; font. By inhibiting this, we easily halve startup times with fonts that are
 ;; larger than the system default.
 frame-inhibit-implied-resize t
 ;; After startup `gcmh' will reset this.
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.6)

;; Faster to disable these here (before they've been initialized)
(push '(scroll-bar-mode . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(alpha . (90 . 75)) default-frame-alist)
;; (push '(fullscreen . maximized) initial-frame-alist)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; TODO load font and theme here
