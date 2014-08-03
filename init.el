;; Package manager preconfig
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
;(add-to-list 'package-archives
;             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;; Package manager init
(package-initialize)

;; Custom keybindings (C = ctrl || s = super)
(global-set-key [C-kp-add] 'text-scale-increase)
(global-set-key [C-kp-subtract] 'text-scale-decrease)
(global-set-key (kbd "s-g") 'save-buffer) ; save
(global-set-key (kbd "s-z") 'undo) ; undo
(global-set-key (kbd "s-x") 'clipboard-kill-region) ; cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ; copy
(global-set-key (kbd "s-v") 'clipboard-yank) ; paste
(global-set-key [C-tab] 'hippie-expand) ; autocomplete

;; Color themes
(require 'monokai-theme)
(load-theme 'monokai t)
;(require 'ciberpunk-theme)
;(load-theme 'cyberpunk t)

;; Custom colors
(set-background-color "#1C1C1C")

;; Custom font size
(set-face-attribute 'default nil :height 140)

;; Show line numbers
(require 'nlinum)
(global-nlinum-mode 1)

;; Reload files automatically
(global-auto-revert-mode t)

;; Start company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Start yasnippet
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook 'yas-minor-mode)

;; elpy config
(require 'elpy)
(elpy-enable)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand) ; fix keymap bug
(define-key global-map (kbd "C-c o") 'iedit-mode) ; fix keymap bug
(setq elpy-default-minor-modes ; disable hightlight
      (remove 'highlight-indentation-mode elpy-default-minor-modes))

;; Add header completion for C/C++/ObjC modes
(defun my:c-header-completion-config()
  (require 'company-c-headers)
  (add-to-list 'company-backends 'company-c-headers))
(add-hook 'c-mode-common-hook 'my:c-header-completion-config)

;; iedit config
(define-key global-map (kbd "C-,") 'iedit-mode) ; fix keymap bug

;; Start google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Load irony-mode as company-mode backend for C/C++/ObjC modes
(defun my:irony-mode-config()
  (require 'irony)
  (irony-mode 1)
  (add-to-list 'company-backends 'company-irony)
  (define-key company-mode-map [remap hippie-expand]
    'company-complete))
(add-hook 'c-mode-common-hook 'my:irony-mode-config)

;; Turn on semantic mode and add it to auto-complete
;(semantic-mode 1)
;(defun my:add-semantic-to-autocomplete()
;  (add-to-list 'ac-source 'ac-source-semantic))
;(add-hook 'c-mode-common-hook 'my:add-semantic-to-autocomplete)

;; Clojure CIDER and nREPL config
(add-hook 'clojure-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 1000)
(setq cider-repl-tab-command 'indent-for-tab-command)
(setq cider-prefer-local-resources t)
(add-hook 'cider-repl-mode 'subword-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

;; Paredit mode for Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode 'paredit-mode)

;; Custom CIDER config
(defun my:clojure-mode-config()
  (require 'cider)
  (cider-mode 1)
  (define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
(add-hook 'clojure-mode-hook 'my:clojure-mode-config)

;; Color parenthesis
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Color each identifier
(require 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)

;; Smart parenthesis config
(defun my:parenthesis-config()
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)) ;; highlights matching pairs
(add-hook 'prog-mode-hook 'my:parenthesis-config)

;; Remove whitespace before saving
(defun my:remove-whitespace-config()
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))
(add-hook 'prog-mode-hook 'my:remove-whitespace-config)
