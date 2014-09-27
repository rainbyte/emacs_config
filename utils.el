(defun my:package-install? (package-name)
  (unless package-archive-contents
    (package-refresh-contents))
  (when (not (package-installed-p package-name))
    (package-install package-name)))
