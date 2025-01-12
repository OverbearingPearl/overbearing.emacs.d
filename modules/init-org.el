(add-to-list 'load-path "~/.emacs.d/modules")

(require 'init-utils)

(use-package org-roam
  :ensure t
  :defer t
  :hook (org-mode . org-roam-db-autosync-mode))

(use-package org-roam-ui
  :ensure t
  :defer t
  :after org-roam
  :diminish (org-roam-ui-mode org-roam-ui-follow-mode))

(use-package org-pomodoro
  :ensure t
  :defer t)

(use-package org-edna
  :ensure t
  :defer t
  :diminish (org-edna-mode)
  :init (setq org-edna-use-inheritance t)
  :hook (org-mode . org-edna-mode))

(use-package org-gtd
  :ensure t
  :defer t
  :after org-edna
  :init
  (setq org-gtd-update-ack "3.0.0")
  :custom
  (org-gtd-organize-hooks '(org-gtd-set-area-of-focus
                            org-set-tags-command))
  (org-gtd-areas-of-focus '("Home"
                            "Health"
                            "Family"
                            "Career"
                            "Finance"
                            "Growth"
                            "Recreation"
                            "Spirituality"
                            "Community")))

(use-package sound-wav
  :ensure t
  :defer t
  :after org-gtd)

(use-package org-tree-slide
  :ensure t
  :defer t)

(add-hook 'org-mode-hook (lambda()
                           (if (check-executable "dot")
                               (org-babel-do-load-languages
                                'org-babel-load-languages
                                '((dot . t))))
                           (setq org-hide-leading-stars t)
                           (org-indent-mode 1)))

(add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode)))

(defun ensure-heading-spaces ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ " nil t)
      (let ((start (match-beginning 0)))
        (beginning-of-line)
        (unless (or (bobp) (eq (char-before (1- start)) ?\n))
          (goto-char (1- start))
          (insert "\n"))
        (forward-line)
        (end-of-line)
        (unless (or (eobp) (eq (char-after) ?\n) (looking-at "^:"))
          (insert "\n"))))))

(defun collapse-multiple-blank-lines ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n\n+" nil t)
      (replace-match "\n\n" nil t))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'ensure-heading-spaces nil t)
            (add-hook 'before-save-hook 'collapse-multiple-blank-lines nil t)))

(provide 'init-org)

;; init-org ends
