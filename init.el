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

(use-package diminish
  :ensure t
  :hook (after-init . (lambda ()
                        (diminish 'auto-revert-mode)
                        (diminish 'abbrev-mode)
                        (diminish 'eldoc-mode))))

(use-package smex
  :ensure t
  :init (smex-initialize))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package ace-window
  :ensure t
  :hook (after-init . ace-window-posframe-mode)
  :bind ("C-x o" . 'ace-window)
  :config (set-face-attribute 'aw-leading-char-face nil :height 8.0))

(use-package posframe
  :ensure t)

(use-package magit
  :if (executable-find "git")
  :ensure t)

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :hook (after-init . global-git-gutter-mode))

(use-package wakatime-mode
  :if (executable-find "wakatime")
  :ensure t
  :diminish wakatime-mode
  :hook (after-init . global-wakatime-mode))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :custom (ivy-use-virtual-buffers t))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :hook (after-init . counsel-mode))

(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper-thing-at-point))

(use-package cmake-mode
  :ensure t)

(use-package clang-format
  :ensure t
  :hook (c++-mode . clang-format-on-save-mode)
  :custom
  (clang-format-fallback-style "Google")
  (clang-format-on-save-p 'always))

(use-package lice
  :ensure t
  :custom
  (lice:copyright-holder "OverbearingPearl")
  (lice:default-license "mit"))

(use-package projectile
  :ensure t
  :hook (after-init . projectile-mode)
  :bind ("C-c p" . 'projectile-commander))

(use-package treemacs
  :ensure t
  :hook (after-init . (lambda ()
                        (treemacs-follow-mode 1)
                        (treemacs-filewatch-mode 1)))
  :bind ("M-0" . 'treemacs-select-window))

(use-package company
  :ensure t
  :hook (after-init . company-mode))

(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp-deferred)
         (c-mode . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-idle-delay 0.1)
  (lsp-auto-guess-root t)
  (lsp-keep-workspace-alive nil)
  (lsp-completion-provider :capf)
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :ensure t
  :hook (lsp-mode . lsp-treemacs-sync-mode))

(use-package which-key-posframe
  :ensure t
  :hook (after-init . which-key-posframe-mode))

(use-package ivy-posframe
  :ensure t
  :diminish ivy-posframe-mode
  :hook (after-init . ivy-posframe-mode))

(add-hook 'git-commit-mode-hook
          (progn
            (setq-local truncate-lines nil)
            (setq-local fill-column 72)
            (auto-fill-mode 1)
            (visual-line-mode 1)))

(setq-default indent-tabs-mode nil)

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(winner-mode 1)

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
