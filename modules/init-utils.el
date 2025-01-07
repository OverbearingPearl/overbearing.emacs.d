(provide 'init-utils)

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

;; init-utils
