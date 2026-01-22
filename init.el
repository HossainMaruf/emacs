(setq inhibit-startup-message t)

(menu-bar-mode -1) ; Disable the menu bar
(tool-bar-mode -1) ; Disable the toolbar
(scroll-bar-mode -1) ; Disable visible scrollbar

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-display-line-numbers-mode t)
(set-face-attribute 'default nil :font "Monaco" :height 120)


; Make availble package functions
(require 'package)

; add a new package source
(customize-set-variable 'package-archives
                        (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))
; Initializes package list
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; Only evaluate this when compiling this file
(eval-when-compile
  ; For each package on the list do
  (dolist (package '(use-package diminish bind-key))
    ; Install if not yet installed
    (unless (package-installed-p package)
      (package-install package))
    ; Require package making it available on the rest of the configuration
    (require package)))

; Easy M-x
(use-package counsel :bind(("M-x" . counsel-M-x)))

(global-set-key (kbd "C-M-p") 'counsel-switch-buffer)
(define-key emacs-lisp-mode-map (kbd "C-M-t") 'counsel-load-theme)

; With ivy we can use counsel package
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map 
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-j" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

; Sidebar navigation with extras
(use-package treemacs
  :ensure t  
  :config
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'extended)
  (treemacs-follow-mode -1)
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1))))

; Project management and tools
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1))

; Unifies projectile and treemacs
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

; GIT interface for Emacs
(use-package magit
  :ensure t
  :bind ("C-c m s" . magit-status))

; Makes treemacs show different colors for committed, staged and modified files
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

; Auto-complete interface
(use-package company
  :ensure t
  :diminish company-mode
  :bind ("M-/" . company-complete)  
  :config
  (global-company-mode))
 
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration)))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package lsp-ui)
(use-package lsp-treemacs)
(use-package hydra)

;; Note: The first time you load your configuration on a new machine, you will
;; need to run the following command to load the modeline icons properly
;; M-x all-the-icons-install-fonts
;; In windows system, all the icons should be on C:\Windows\Fonts
;; But we download the icons in .emacs.d/fonts folder
;; So we need to copy the fonts in system requirements directory
(use-package all-the-icons)
; Doom Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
(use-package doom-themes)
; evil mode for vim user
(defun rune/evil-hook()
  (dolist (mode '(custom-mode
		  eshell-mode
		  gitrebase-mode
		  erc-mode
		  circe-server-mode
		  circe-chat-mode
		  circe-query-mode
		  sauron-mode
		  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . rune/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-j") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; use visula line motion even outside visual line mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

; BUILT-IN PACKAGES
(use-package which-key
  :init(which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  ; Set the maximum length (in characters) for key descriptions (commands or
  ; prefixes). Descriptions that are longer are truncated and have ".." added.
  (setq which-key-max-description-length 20)
  ; Use additional padding between columns of keys. This variable specifies the
  ; number of spaces to add to the left of each column.
  (setq which-key-add-column-padding 3)
  ; The maximum number of columns to display in the which-key buffer. nil means
  ; don't impose a maximum.
  (setq which-key-max-display-columns 4))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "b7a09eb77a1e9b98cafba8ef1bd58871f91958538f6671b22976ea38c2580755"
     default))
 '(package-selected-packages
   '(all-the-icons company counsel diminish doom-modeline doom-themes
		   evil lsp-java lsp-ui treemacs-magit
		   treemacs-projectile yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
