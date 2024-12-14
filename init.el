(defun load-file-when-exists (custom-file)
  (if (file-exists-p custom-file)
      (load custom-file nil :noerror)))

(load-file-when-exists "~/.emacs.d/custom/custom-prelude.el")

(setq custom-file "~/.emacs.d/custom/custom.el")
(load-file-when-exists custom-file)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/") t)
(add-to-list 'package-archives
             '("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-check-signature nil) ; for speeding up
(add-hook 'after-init-hook #'package-autoremove)

(use-package magit
  :ensure t)

(use-package wakatime-mode
  :ensure t
  :hook (after-init . global-wakatime-mode))

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package counsel
  :ensure t
  :hook (after-init . counsel-mode))

(use-package cmake-mode
  :ensure t)

(add-hook 'git-commit-mode-hook
          (progn
            (setq-local truncate-lines nil)
            (setq-local fill-column 72)
            (auto-fill-mode 1)
            (visual-line-mode 1)))

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defun resize-frame-centered (frame)
  (with-selected-frame frame
    (let* ((screen-width (display-pixel-width))
           (screen-height (display-pixel-height))
           (screen-center-x (/ screen-width 2))
           (screen-center-y (/ screen-height 2))
           (frame-width (frame-pixel-width))
           (frame-height (frame-pixel-height))
	   (frame-left (- screen-center-x (/ frame-width 2)))
	   (frame-top (- screen-center-y (/ frame-height 2))))
      (set-frame-position frame frame-left frame-top))))
(defun resize-current-frame-centered ()
  (resize-frame-centered (selected-frame)))

(add-hook 'after-make-frame-functions 'resize-frame-centered)
(add-hook 'after-init-hook 'resize-current-frame-centered)

(server-start)

(load-file-when-exists "~/.emacs.d/custom/custom-coda.el")

;; .init.el ends
