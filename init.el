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

;; Start and config auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;; Start yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; elpy config
(require 'elpy)
(elpy-enable)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand) ; fix keymap bug
(define-key global-map (kbd "C-c o") 'iedit-mode) ; fix keymap bug
(setq elpy-default-minor-modes ; disable hightlight
      (remove 'highlight-indentation-mode elpy-default-minor-modes))

;; Add ac-complete-headers to C/C++/ObjC modes
(defun my:ac-c-header-init()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)
(add-hook 'objc-mode-hook 'my:ac-c-header-init)

;; iedit config
(define-key global-map (kbd "C-,") 'iedit-mode) ; fix keymap bug

;; Start google-c-style
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; Load company-mode + irony-mode for C/C++/ObjC
(defun my:company-mode-hook()
  (require 'irony)
  (irony-mode 1)
  (require 'company)
  (company-mode 1)
  (add-to-list 'company-backends 'company-irony)
  (define-key company-mode-map [remap hippie-expand]
    'company-complete))
(add-hook 'c++-mode-hook 'my:company-mode-hook)
(add-hook 'c-mode-hook 'my:company-mode-hook)
(add-hook 'objc-mode-hook 'my:company-mode-hook)

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
(defun my:clojure-mode-hook()
  (require 'cider)
  (cider-mode 1)
  (define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
(add-hook 'clojure-mode-hook 'my:clojure-mode-hook)

;; Color parenthesis
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Color each identifier
(require 'rainbow-identifiers)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
