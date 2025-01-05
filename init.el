(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(defun load-file-when-exists (custom-file)
  (if (file-exists-p custom-file)
      (load custom-file nil :noerror)))

(defun check-executable (my-executable &optional min-version my-executable-path)
  (defun extract-version-from-string (version-string)
    (let ((version ""))
      (when (string-match "\\([^0-9]+\\)?\\([0-9]+\\)\\(\\..*\\)?"
                          version-string)
        (setq version (match-string 2 version-string)))
      version))
  (let ((executable (or (and (boundp my-executable-path)
                             (symbol-value my-executable-path))
                        (executable-find my-executable))))
    (if executable
        (let* ((version-command (format "%s --version"
                                        (shell-quote-argument executable)))
               (version-string (shell-command-to-string version-command))
               (version (extract-version-from-string version-string)))
          (if min-version
              (version<= min-version version)
            t))
      nil)))

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

(use-package benchmark-init
  :ensure t
  :config (progn
            (setq gc-cons-threshold most-positive-fixnum)
            (benchmark-init/activate)
            (defun my-after-init-tasks ()
              (benchmark-init/deactivate)
              (setq gc-cons-threshold (* 100 1024 1024)))
            (add-hook 'after-init-hook 'my-after-init-tasks)))

(use-package diminish
  :ensure t
  :hook ((auto-revert-mode . (lambda () (diminish 'auto-revert-mode)))
         (abbrev-mode . (lambda () (diminish 'abbrev-mode)))
         (eldoc-mode . (lambda () (diminish 'eldoc-mode)))))

(use-package smex
  :ensure t
  :init (smex-initialize))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package magit
  :ensure t
  :defer t)

(use-package gitignore-templates
  :ensure t
  :defer t)

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :hook (after-init . global-git-gutter-mode))

(use-package wakatime-mode
  :when (check-executable "wakatime")
  :ensure t
  :diminish wakatime-mode
  :hook (after-init . global-wakatime-mode))

(use-package immersive-translate
  :ensure t
  :defer t
  :custom (immersive-translate-backend 'baidu))

(use-package ox-hugo
  :ensure t
  :defer t)

(use-package easy-hugo
  :when (check-executable "hugo" nil 'easy-hugo-bin)
  :ensure t
  :defer t
  :custom
  (easy-hugo-default-ext ".org"))

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
  :ensure t)

(use-package protobuf-mode
  :ensure t
  :defer t
  :mode ("\\.proto$" . protobuf-mode))

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package clang-format
  :when (check-executable "clang-format" "19" 'clang-format-executable)
  :ensure t
  :defer t
  :hook (c++-mode . clang-format-on-save-mode)
  :custom
  (clang-format-fallback-style "Google")
  (clang-format-on-save-p 'always))

(use-package lice
  :ensure t
  :defer t
  :custom
  (lice:copyright-holder "OverbearingPearl")
  (lice:default-license "mit"))

(use-package projectile
  :ensure t
  :defer t
  :hook (after-init . projectile-mode)
  :bind ("C-c p" . 'projectile-commander))

(use-package treemacs
  :ensure t
  :defer t
  :bind ("M-0" . 'treemacs-select-window)
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1))

(use-package company
  :ensure t
  :diminish company-mode
  :hook (after-init . global-company-mode))

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((c++-mode . lsp-deferred)
         (c-mode . lsp-deferred))
  :custom
  (lsp-idle-delay 0.1)
  (lsp-auto-guess-root t)
  (lsp-keep-workspace-alive nil)
  (lsp-completion-provider :capf)
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :ensure t
  :defer t
  :hook (lsp-mode . lsp-treemacs-sync-mode))

(use-package python
  :ensure t
  :defer t)

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package poetry
  :when (check-executable "poetry")
  :ensure t
  :defer t)

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '((:sunrise . default)
                           ("21:30"  . modus-vivendi)))
  (circadian-setup))

(add-to-list 'load-path "~/.emacs.d/modules")
(require 'init-org)

(add-hook 'git-commit-mode-hook
          (progn
            (setq-local truncate-lines nil)
            (setq-local fill-column 72)
            (magit-auto-revert-mode 1)
            (require 'git-commit)
            (git-commit-mode 1)
            (auto-fill-mode 1)
            (visual-line-mode 1)))

(setq-default indent-tabs-mode nil)

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(winner-mode 1)
(setq use-short-answers t)
(setq visible-bell t)

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

(cond
 ((eq system-type 'windows-nt)
  (set-frame-font "Consolas" nil t))
 ((eq system-type 'darwin)
  (set-frame-font "Monaco" nil t))
 ((eq system-type 'gnu/linux)
  (set-frame-font "DejaVu Sans Mono" nil t))
 (t
  (set-frame-font "Noto Mono" nil t)))

(server-start)

(load-file-when-exists "~/.emacs.d/custom/custom-coda.el")

;; init.el ends
