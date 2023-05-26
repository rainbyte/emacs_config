;;; init.el --- Initialization file for Emacs

;;; Commentary:
;; Emacs Startup File --- initialization for Emacs

;;; Code:

;; Independent custom-set-* config
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" "" custom-file))
(load custom-file)


;; Package manager preconfig
(require 'package)
(setq package-enable-at-startup nil)    ; disable package auto-load
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)


;; Select stable package versions (before package-init!)
(setq package-archive-priorities
      '(("melpa-stable" . 100)
        ("melpa" . 50)))
(setq package-pinned-packages
      '((lispy . "melpa")))


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


;; Enable standard copy & paste
(cua-mode t)


;; Default shell cmd, various packages assume bash
(setq shell-file-name "bash")


;; Fix undefined dead-accent keys issue
(require 'iso-transl)


;; Color themes
(use-package monokai-theme ; ciberpunk-theme
  :ensure t
  :init (load-theme 'monokai t))



;; UI: custom colors
(set-background-color "#000000")


;; UI: menu colors on console TUI
(set-face-attribute 'menu nil
                    :inverse-video nil
                    :background "green"
                    :foreground "magenta"
                    :bold t)


;; Custom font size
;; (set-face-attribute 'default nil :height 140)


;; Spacemacs-like powerline
(use-package spaceline
  :ensure t
  :functions spaceline-emacs-theme spaceline-toggle-minor-modes-off
  :if window-system                     ; enable only in gui mode
  :init
  (setq-default powerline-default-separator 'wave)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off))


;; Automatic resizing
(use-package golden-ratio
  :ensure t
  :disabled t
  :config
  (golden-ratio-mode t)
  (golden-ratio-toggle-widescreen))


;; Show line numbers
(use-package nlinum
  :ensure t
  :disabled t
  :config
  (global-nlinum-mode 1))


;; UI: hide desktop window decorations
(modify-all-frames-parameters '((undecorated t)))


;; Custom UI options
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(setq-default indicate-buffer-boundaries 'right)


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
(defun my:text-mode-config ()
  "Tabs width workaround."
  (setq indent-line-function 'insert-tab))
(add-hook 'text-mode-hook 'my:text-mode-config)


;; Neotree, filesystem navigation
(use-package neotree
  :ensure t
  :bind ([f8] . neotree-toggle)
  :config
  (setq neo-autorefresh nil))


;; Consistent coding styles
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode t))


;; On the fly syntax checking
(use-package flycheck
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'text-mode-hook 'flycheck-mode))


;; Start company-mode
(use-package company
  :ensure t
  :defines company-dabbrev-downcase
  :init
  (setq company-tooltip-align-annotations t)
  (setq company-dabbrev-downcase nil) ; case sensitive text completions
  (setq company-show-numbers t)       ; visual numbering of candidates
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  ;; Make tab indentation and completion work properly
  (define-key company-mode-map [remap indent-for-tab-command]
              'company-indent-for-tab-command)
  (setq tab-always-indent 'complete) ; indent first, and then complete
  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let* ((functions-saved completion-at-point-functions)
           (completion-at-point-functions
            (lambda ()
              (let ((completion-at-point-functions functions-saved))
                (when (fboundp 'company-complete-common)
                  (company-complete-common))))))
      (indent-for-tab-command arg)))

  (use-package company-quickhelp
    :ensure t
    :config
    (add-hook 'company-mode-hook 'company-quickhelp-mode)))


;; Start yasnippet
(use-package yasnippet
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode))


;; Highlight lines with 80+ characters
(use-package column-enforce-mode
  :ensure t
  :config
  (set-face-attribute 'column-enforce-face nil :background "#880000")
  (add-hook 'prog-mode-hook 'column-enforce-mode))


;; Color parenthesis
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode))


;; Color each identifier
(use-package rainbow-identifiers
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))


;; Smart parenthesis config
(use-package smartparens
  :ensure t
  :defer t
  :init
  (defun my-smartparens-config ()
    (require 'smartparens-config)
    (smartparens-global-mode t)       ;; smart completion
    (show-smartparens-global-mode t)) ;; highlights matching pairs
  (add-hook 'prog-mode-hook 'my-smartparens-config))


;; Remove whitespace before saving
(defun my:remove-whitespace-config ()
  "Remove whitespace before saving."
  (unless (derived-mode-p 'markdown-mode)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my:remove-whitespace-config)


;; ---------------------------------------------------------
;; ------------------ Languages & formats ------------------
;; ---------------------------------------------------------


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
    (add-to-list 'company-backends
                 '(company-anaconda :with company-capf)))
  (use-package flycheck-mypy
    :ensure t
    :config
    (when (fboundp 'flycheck-add-next-checker)
      (flycheck-add-next-checker 'python-pycompile 'python-mypy)
      (flycheck-add-next-checker 'python-flake8 'python-mypy))))


;; Add header completion for C/C++/ObjC modes
(use-package company-c-headers
  :ensure t
  :defer t
  :init
  (defun my-company-c-headers-hook ()
    (add-to-list 'company-backends 'company-c-headers))
  (dolist (a-mode-hook '(c-mode-hook objc-mode-hook c++-mode-hook))
    (add-hook a-mode-hook 'my-company-c-headers-hook)))


;; Load irony-mode as company-mode backend for C/C++/ObjC modes
(use-package irony
  :ensure t
  :defer t
  :init
  (dolist (a-mode-hook '(c-mode-hook objc-mode-hook c++-mode-hook))
    (add-hook a-mode-hook 'irony-mode))
  :config
  (use-package company-irony
    :ensure t
    :config
    (defun my-company-irony-hook()
      (add-to-list 'company-backends 'company-irony)
      (define-key company-mode-map [remap hippie-expand]
        'company-complete))
    (add-hook 'irony-mode-hook 'my-company-irony-hook))
  (use-package flycheck-irony
    :ensure t
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  (use-package irony-eldoc
    :ensure t
    :config
    (add-hook 'irony-mode-hook 'irony-eldoc)))


;; Clojure mode config
(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (use-package cider
    :ensure t)
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode) ; show docs inside minibuffer
  (defun my-cider-repl-mode-config ()
    (setq nrepl-hide-special-buffers t)
    (setq nrepl-log-messages t)
    (setq cider-repl-wrap-history t)
    (setq cider-repl-history-size 1000))
  (add-hook 'cider-repl-mode-hook 'my-cider-repl-mode-config)
  (add-hook 'cider-repl-mode-hook 'subword-mode))


;; Enhanced ruby mode config
(use-package ruby-mode
  :mode "\\.rb$"
  :config
  (use-package enh-ruby-mode :ensure t)
  (use-package robe :ensure t)
  (use-package yard-mode :ensure t)
  (defun my-ruby-mode-config ()
    (rainbow-identifiers-mode 0)      ; fixes wrong colors
    (robe-mode 1)
    (push 'company-robe company-backends)
    (yard-mode 1))
  (add-hook 'enh-ruby-mode-hook 'my-ruby-mode-config))


;; CSV mode config
(use-package csv-mode
  :ensure t
  :mode "\\.csv$"
  :init
  (setq csv-separators '(";")))


;; Racket and Scheme interaction mode
(use-package geiser
  :ensure t
  :defer t
  :defines geiser-mode-start-repl-p
  :init
  (defun my-geiser-mode-hook ()
    (setq geiser-mode-start-repl-p t) ; auto-start repl
    (push 'geiser-company-backend company-backends)
    (geiser-mode t))
  (add-hook 'scheme-mode-hook 'my-geiser-mode-hook))


;; Vala config
(use-package vala-mode
  :ensure t
  :mode (("\\.vala$" . vala-mode)
         ("\\.vapi$" . vala-mode))
  :config
  (use-package vala-snippets
    :ensure t)
  (add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
  (add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))
  (defun my-vala-mode-config()
    (run-hooks 'prog-mode-hook)
    (setq c-basic-offset 4)
    (c-set-style "java"))
  (add-hook 'vala-mode-hook 'my-vala-mode-config))


;; CMake support
(use-package cmake-mode
  :ensure t
  :defer t)


;; Haskell support
(use-package haskell-mode
  :ensure t
  :defer t
  :init
  (dolist (hook '(eldoc-mode            ; types on minibuffer
                  haskell-decl-scan-mode
                  interactive-haskell-mode
                  ghc-comp-init))
    (add-hook 'haskell-mode-hook hook)))
(use-package ghc
  :ensure t
  :commands ghc-comp-init ghc-init ghc-debug
  :config
  (use-package company-ghc
    :ensure t
    :config
    (add-to-list 'company-backends 'company-ghc)))


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
  :mode "\\.html?"
  :config
  (setq web-mode-auto-close-style 2)) ;; Tag auto-close with > and </.


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

;; C# support
(use-package csharp-mode
  :ensure t
  :defer t)


;; JavaScript support
(use-package tern
  :ensure t
  :defer t
  :commands tern-mode
  :config
  (use-package company-tern
    :ensure t
    :config
    (add-to-list 'company-backends 'company-tern)))
(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js$"
  :init
  (add-hook 'js2-mode-hook 'tern-mode))


;; JSON support
(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json$")


;; F#
(use-package fsharp-mode
  :ensure t
  :defer t)


;; Yaml
(use-package yaml-mode
  :ensure t
  :defer t)


;; Ledger
(use-package ledger-mode
  :ensure t
  :defer t)


(provide 'init)
;;; init.el ends here
