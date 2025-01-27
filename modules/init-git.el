(use-package magit
  :ensure t
  :mode ("[A-Z]*_\\(EDIT\\)?MSG\\'" . git-commit-mode)
  :config (magit-auto-revert-mode 1)
  :hook (git-commit . (lambda ()
                        (setq-local truncate-lines nil)
                        (setq-local fill-column 72)
                        (auto-fill-mode 1)
                        (visual-line-mode 1))))

(use-package gitignore-templates
  :ensure t
  :defer t)

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :hook (after-init . global-git-gutter-mode))

(use-package lab
  :when (string-match "JSON" system-configuration-features)
  :ensure t
  :defer t)

(provide 'init-git)

;; init-git ends
