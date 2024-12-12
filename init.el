(defun load-file-when-exists (custom-file)
  (if (file-exists-p custom-file)
      (load custom-file nil :noerror)))

(load-file-when-exists "~/.emacs.d/custom/custom-prelude.el")

(setq custom-file "~/.emacs.d/custom/custom.el")
(load-file-when-exists custom-file)

(require 'package)

;; 设置中国科学技术大学的 Emacs 镜像源
(add-to-list 'package-archives
             '("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/") t)

;; 设置 GNU Emacs 镜像源
(add-to-list 'package-archives
             '("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/") t)

;; 初始化包管理器
(package-initialize)

;; 确保 use-package 已经安装
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq package-check-signature nil) ; 禁用签名检查，加快更新速度
(add-hook 'after-init-hook #'package-autoremove) ; 删除不再使用的包

(use-package wakatime-mode
  :ensure t
  :config
  (global-wakatime-mode 1))

(use-package magit
  :ensure t)

(add-hook 'git-commit-mode-hook
          (progn
            (setq-local truncate-lines nil)
            (setq-local fill-column 72)
            (auto-fill-mode 1)
            (visual-line-mode 1)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(defun resize-frame-centered (frame)
  "Set the Emacs frame to be centered on the screen."
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

;; 将函数添加到 after-make-frame-functions
(add-hook 'after-make-frame-functions 'resize-frame-centered)

(server-start)

(load-file-when-exists "~/.emacs.d/custom/custom-coda.el")

;; .init.el ends
