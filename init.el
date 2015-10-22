;; Utilitary functions
(load "~/.emacs.d/utils.el")


;; Package manager preconfig
(require 'package)
(setq package-enable-at-startup nil)    ; disable package auto-load
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)


;; Select stable package versions (before package-init!)
(setq package-pinned-packages '((clojure-mode . "melpa-stable")
                                (cider . "melpa-stable")
                                (geiser . "melpa-stable")
                                (slime . "melpa-stable")
                                (slime-company . "melpa-stable")
                                (lispy . "melpa")))


;; Package manager init
(package-initialize)


;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;; Custom keybindings (C = ctrl || s = super)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key [C-kp-add] 'text-scale-increase)
(global-set-key [C-kp-subtract] 'text-scale-decrease)
(global-set-key (kbd "s-g") 'save-buffer) ; save
(global-set-key (kbd "s-z") 'undo) ; undo
(global-set-key (kbd "s-x") 'clipboard-kill-region) ; cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ; copy
(global-set-key (kbd "s-v") 'clipboard-yank) ; paste
(global-set-key [C-tab] 'hippie-expand) ; autocomplete
(define-key global-map (kbd "C-,") 'iedit-mode) ; iedit, fix keymap bug


;; Color themes
(use-package monokai-theme ; ciberpunk-theme
  :ensure t
  :init (load-theme 'monokai t))


;; Custom colors
;(set-background-color "#1C1C1C")


;; Custom font size
;(set-face-attribute 'default nil :height 140)


;; Vim-like powerline
(use-package smart-mode-line
  :ensure t
  :config
  (use-package smart-mode-line-powerline-theme
    :ensure t
    :if window-system                   ; enable only in gui mode
    :config
    (setq sml/theme 'powerline))
  (setq rm-whitelist '(""))
  (sml/setup))


;; Automatic resizing
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode t)
  (golden-ratio-toggle-widescreen))


;; Show line numbers
(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode 1))


;; Show column number
(setq column-number-mode t)


;; Reload files automatically
(global-auto-revert-mode t)


;; Preserve mini-buffer history
(savehist-mode 1)


;; Indentation setup
(setq-default indent-tabs-mode nil) ; use spaces only, never tab
(setq-default tab-width 4) ; 4 spaces
(setq-default c-default-style "java")
(setq-default c-basic-offset 4)


;; Text-mode tabs indentation
(defun my:text-mode-config()
  ;; Tabs width workaround
  (setq indent-line-function 'insert-tab))
(add-hook 'text-mode-hook 'my:text-mode-config)


;; Neotree, filesystem navigation
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle))


;; On the fly syntax checking
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'text-mode-hook 'flycheck-mode))


;; Start company-mode
(my:package-install? 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-tooltip-align-annotations t)


;; Make tab indentation and completion work properly
(define-key company-mode-map [remap indent-for-tab-command]
  'company-indent-for-tab-command)
(setq tab-always-indent 'complete)      ; indent first, and then complete
(defun company-indent-for-tab-command (&optional arg)
  (interactive "P")
  (let* ((functions-saved completion-at-point-functions)
         (completion-at-point-functions
          (lambda ()
            (let ((completion-at-point-functions functions-saved))
              (company-complete-common)))))
    (indent-for-tab-command arg)))


;; Show documentation inside company popup
(my:package-install? 'company-quickhelp)
(require 'company-quickhelp)
(add-hook 'company-mode-hook 'company-quickhelp-mode)


;; Start yasnippet
(my:package-install? 'yasnippet)
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook 'yas-minor-mode)


;; Highlight lines with 80+ characters
(use-package column-enforce-mode
  :ensure t
  :config
  (set-face-attribute 'column-enforce-face nil :foreground "#ff0000")
  (add-hook 'prog-mode-hook 'column-enforce-mode))


;; Python support
(use-package anaconda-mode
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'eldoc-mode)
  :config
  (use-package company-anaconda
    :ensure t
    :config
    (add-to-list 'company-backends 'company-anaconda)))


;; Add header completion for C/C++/ObjC modes
(my:package-install? 'company-c-headers)
(require 'company-c-headers)
(defun my:c-header-completion-config()
  (add-to-list 'company-backends 'company-c-headers))
(add-hook 'c-mode-hook 'my:c-header-completion-config)
(add-hook 'objc-mode-hook 'my:c-header-completion-config)
(add-hook 'c++-mode-hook 'my:c-header-completion-config)


;; Start google-c-style
;(my:package-install? 'google-c-style)
;(require 'google-c-style)
;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;; Load irony-mode as company-mode backend for C/C++/ObjC modes
(my:package-install? 'irony)
(require 'irony)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)

(my:package-install? 'company-irony)
(require 'company-irony)
(defun my:irony-mode-config()
  (add-to-list 'company-backends 'company-irony)
  (define-key company-mode-map [remap hippie-expand]
    'company-complete))
(add-hook 'irony-mode-hook 'my:irony-mode-config)

(my:package-install? 'flycheck-irony)
(require 'flycheck-irony)
(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)

(my:package-install? 'irony-eldoc)
(require 'irony-eldoc)
(add-hook 'irony-mode-hook 'irony-eldoc)


;; Turn on semantic mode and add it to auto-complete
;(semantic-mode 1)
;(defun my:add-semantic-to-autocomplete()
;  (add-to-list 'ac-source 'ac-source-semantic))
;(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)


;; Clojure mode config
(my:package-install? 'clojure-mode)
(require 'clojure-mode)
(my:package-install? 'cider)
(require 'cider)
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-mode-hook 'eldoc-mode) ; documentation inside minibuffer


;;; Clojure repl config
(defun my:cider-repl-mode-config ()
  (setq nrepl-hide-special-buffers t)
  (setq nrepl-log-messages t)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 1000))
(add-hook 'cider-repl-mode-hook 'my:cider-repl-mode-config)
(add-hook 'cider-repl-mode-hook 'subword-mode)


;; Color parenthesis
(my:package-install? 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)


;; Color each identifier
(my:package-install? 'rainbow-identifiers)
(require 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)


;; Smart parenthesis config
(my:package-install? 'smartparens)
(require 'smartparens)
(defun my:parenthesis-config()
  (require 'smartparens-config)
  (smartparens-global-mode t) ;; smart completion
  (show-smartparens-global-mode t)) ;; highlights matching pairs
(add-hook 'prog-mode-hook 'my:parenthesis-config)


;; Remove whitespace before saving
(defun my:remove-whitespace-config ()
  (unless (derived-mode-p 'markdown-mode)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my:remove-whitespace-config)


;; Enhanced ruby mode config
(my:package-install? 'enh-ruby-mode)
(require 'enh-ruby-mode)
(my:package-install? 'robe)
(require 'robe)
(my:package-install? 'yard-mode)
(require 'yard-mode)
(defun my:ruby-mode-config()
  (rainbow-identifiers-mode 0) ; fixes wrong colors
  (robe-mode 1)
  (push 'company-robe company-backends)
  (yard-mode 1))
(add-hook 'enh-ruby-mode-hook 'my:ruby-mode-config)


;; CSV mode config
(my:package-install? 'csv-mode)
(require 'csv-mode)
(defun my:csv-mode-config()
  (setq csv-separators '(";")))
(add-hook 'csv-mode-hook 'my:csv-mode-config)


;; Racket and Scheme interaction mode
(use-package geiser
  :ensure t
  :defer t
  :init
  (defun my-geiser-mode-hook ()
    (setq geiser-mode-start-repl-p t) ; auto-start repl
    (push 'geiser-company-backend company-backends)
    (geiser-mode t))
  (add-hook 'scheme-mode-hook 'my-geiser-mode-hook))


;; Vala config
(my:package-install? 'vala-mode)
(require 'vala-mode)
(my:package-install? 'vala-snippets)
(require 'vala-snippets)
(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))
(defun my:vala-mode-config()
  (run-hooks 'prog-mode-hook)
  (setq c-basic-offset 4)
  (c-set-style "java"))
(add-hook 'vala-mode-hook 'my:vala-mode-config)


;; CMake support
(use-package cmake-mode
  :ensure t
  :defer t)


;; Haskell support
(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (dolist (hook '(turn-on-haskell-doc-mode ; types on minibuffer
                  turn-on-haskell-indent   ; semi-automatic tab
                  interactive-haskell-mode))
    (add-hook 'haskell-mode-hook hook)))


;; PKGBUILD mode
(use-package pkgbuild-mode
  :ensure t
  :mode "PKGBUILD")


;; Superior Lisp Interaction Mode for Emacs
(use-package slime
  :ensure t
  :defer t
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (use-package slime-company
    :ensure t)
  (add-hook 'slime-mode-hook
            (lambda ()
              (load (expand-file-name "~/quicklisp/slime-helper.el"))
              (add-to-list 'slime-contribs 'slime-fancy)
              (add-to-list 'slime-contribs 'inferior-slime))))


;; vi-like paredit
(use-package lispy
  :ensure t
  :defer t
  :init
  (dolist (a-mode-hook '(clojure-mode-hook
                         emacs-lisp-mode-hook
                         lisp-mode-hook
                         scheme-mode-hook))
    (add-hook a-mode-hook 'lispy-mode)))


;; markdown support
(use-package markdown-mode
  :ensure t
  :defer t)


;; web templates mode
(use-package web-mode
  :ensure t
  :mode "\\.html?")


;; rust support
(use-package rust-mode
  :ensure t
  :defer t
  :config
  (use-package flycheck-rust
    :ensure t
    :config (flycheck-rust-setup))
  (use-package racer
    :ensure t
    :init
    (unless (getenv "RUST_SRC_PATH")
      (setenv "RUST_SRC_PATH"
              (expand-file-name "/usr/src/rust/src")))
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode)))


(provide 'init)
;;; init.el ends here
