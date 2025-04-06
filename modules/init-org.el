(add-to-list 'load-path "~/.emacs.d/modules")

(require 'init-utils)

(use-package org-roam
  :when (string-match "SQLITE3" system-configuration-features)
  :ensure t
  :defer t
  :config
  (add-hook 'after-save-hook
            (lambda ()
              (when (string-match-p (expand-file-name org-roam-directory)
                                    (buffer-file-name))
                (org-roam-db-sync)))))

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

(defun ensure-heading-spaces ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ " nil t)
      (let ((start (match-beginning 0)))
        (beginning-of-line)
        (unless (or (bobp) (eq (char-before (1- start)) ?\n))
          (goto-char (1- start))
          (insert "\n")
          (forward-line))
        (forward-line)
        (beginning-of-line)
        (unless (or (eq (char-after) ?\n)
                    (looking-at "^:")
                    (looking-at "^DEADLINE: ")
                    (looking-at "^SCHEDULED: "))
          (end-of-line)
          (unless (eobp)
            (beginning-of-line)
            (insert "\n")))))))

(defun collapse-multiple-blank-lines ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n\n+" nil t)
      (replace-match "\n\n" nil t))))

(add-hook 'org-mode-hook
          (lambda ()
            (if (check-executable "dot")
                (org-babel-do-load-languages 'org-babel-load-languages '((dot . t))))
            (if (boundp 'org-plantuml-jar-path)
                (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))
            (setq org-image-actual-width `(,(* (frame-char-width) 72)))
            (setq org-startup-with-inline-images t)
            (add-hook 'after-save-hook 'org-redisplay-inline-images)
            (setq org-confirm-babel-evaluate nil)
            (setq org-hide-leading-stars t)
            (add-hook 'org-indent-mode-hook (lambda () (diminish 'org-indent-mode)))
            (add-hook 'before-save-hook 'ensure-heading-spaces nil t)
            ;; (add-hook 'before-save-hook 'collapse-multiple-blank-lines nil t)
            (org-indent-mode 1)))

(provide 'init-org)

;; init-org ends
