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
  :init (setq org-edna-use-inheritance t)
  :hook (org-mode . org-edna-mode))

(use-package org-gtd
  :ensure t
  :after org-edna
  :init
  (setq org-gtd-update-ack "3.0.0")
  :config
  (setq org-gtd-areas-of-focus (append org-gtd-areas-of-focus
                                       '("Finance"
                                         "Growth"
                                         "Recreation"
                                         "Spirituality"
                                         "Community"))))

(provide 'init-org)

;; init-org ends
