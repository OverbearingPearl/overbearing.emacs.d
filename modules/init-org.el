(add-to-list 'load-path "~/.emacs.d/modules")

(require 'init-utils)

(use-package org-roam
  :ensure t
  :defer t
  :hook (org-mode . org-roam-db-autosync-mode))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :diminish (org-roam-ui-mode org-roam-ui-follow-mode))

(use-package sound-wav
  :ensure t)

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

(setq org-hide-leading-stars t)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode)))

(if (check-executable "dot")
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t))))

(provide 'init-org)

;; init-org ends
