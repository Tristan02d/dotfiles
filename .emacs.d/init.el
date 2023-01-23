;; -*- lexical-binding: t; -*-

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

(setq server-client-instructions nil)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(setq inhibit-startup-message t) ;; Enlever l'écran d'accueil
(scroll-bar-mode -1)  ;; Enlever la scrollbar
(tool-bar-mode -1)    ;; Enlever la barre d'outils
(tooltip-mode -1)     ;; Enlever les tooltips
(menu-bar-mode -1)    ;; Enlever la barre de menu
(save-place-mode 1)   ;; Mémoriser le dernier emplacement du curseur

;; Enable line numbers for some modes
(dolist (mode '(prog-mode-hook
                lsp-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(column-number-mode 1)

;; Initialisation des packages
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; Raccourcis clavier
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "M-C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "M-C-v") 'eval-region)
(global-set-key (kbd "M-C-g a") 'org-agenda)
(global-set-key (kbd "M-C-g c") 'org-capture)
(global-set-key (kbd "C-x K") 'kill-current-buffer)
(global-set-key (kbd "C-$") 'delete-other-windows)

;; Mettre automatiquement les parenthèses fermantes
(electric-pair-mode 1)

;; Ne pas confirmer la fermeture des buffers ayant un processus
(setq kill-buffer-query-functions nil)

(defun dw/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

(use-package evil
  :init
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Disable arrow keys in normal and visual modes
  ;;   (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
  ;;   (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
  ;;   (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
  ;;   (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
  ;;   (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
  ;;   (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
  ;;   (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
  ;;   (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)  ;; Is this a bug in evil-collection?
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (delete 'lispy evil-collection-mode-list)
  (delete 'org-present evil-collection-mode-list)
  (evil-collection-init))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Infos sur les raccourcis clavier
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(defun tr/set-faces ()
  (set-face-attribute 'default nil
                      :font "Ubuntu Mono"
                      :height 128))

;; Themes
(use-package monokai-pro-theme)
(use-package solo-jazz-theme)

(advice-add 'rainbow-turn-on :after  #'solo-jazz-theme-rainbow-turn-on)
(advice-add 'rainbow-turn-off :after #'solo-jazz-theme-rainbow-turn-off)

;; Récupérer l'heure qu'il est au lancement pour charger un theme en conséquence
(defun tr/set-theme ()
  (setq hour-at-start (decoded-time-hour (decode-time)))
  (if (and (<= 7 hour-at-start)
	   (>= 17 hour-at-start))

      (load-theme 'solo-jazz t)

    (load-theme 'monokai-pro t)
    )
  )

(defun tr/emacs-client-frame-setup ()
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (tr/set-theme)
                (tr/set-faces)
                (setq doom-modeline-icon t)
                (toggle-frame-fullscreen)))))

(defun tr/emacs-gui-frame-setup ()
  (toggle-frame-fullscreen)
  (tr/set-theme)
  (tr/set-faces))

(use-package all-the-icons)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (dired-kill-when-opening-new-dired-buffer t)
           (evil-collection-define-key 'normal 'dired-mode-map
             "h" 'dired-single-up-directory
             "l" 'dired-single-buffer)))

(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :config
  (setq dired-open-extensions '(("png" . "gimp")
                                ("pdf" . "evince"))))

(use-package diminish)

(use-package doom-modeline
:custom
(display-time-mode 1)
(display-battery-mode 1)
(doom-modeline-height 1)
(setq doom-modeline-buffer-encoding nil)
(doom-modeline-mode 1))

(use-package expand-region
  :bind (("M-[" . er/expand-region)
         ("C-(" . er/mark-outside-pairs)))

;; Parenthèses plus lisibles
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package beacon
  :init
  (beacon-mode 1))

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
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-load-theme)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Meilleure description des commandes/touches
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Ajouter la notion de projet à Emacs
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projets/")
    (setq projectile-project-search-path '("~/projets/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Org -----------------------------------------------------------------

;; Turn on indentation and auto-fill mode for Org files
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :commands (org-capture org-agenda)
  :config
  (setq org-support-shift-select t)
  (setq org-ellipsis " ▾")

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/.dotfiles/agenda.org"
          "~/projets/tristank/TODO.org"))

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-tag-alist
        '((:startgroup)
                                        ; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))

  (efs/org-font-setup))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (C . t)
     (shell . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cc" . "src C")))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal buffer-file-name "/home/tristan/.dotfiles/emacs.org")
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package org-make-toc
:hook (org-mode . org-make-toc-mode))

(use-package markdown-mode
  :ensure nil
  :hook (markdown-mode . efs/visual-fill))

(use-package grip-mode)

;; Complétion de code et autres ----------------------------------------

;; Package qui sert à créer des espèces d'alias personnalisés
;; pour programmer plus vite
(use-package yasnippet
  :hook (lsp-mode . yas-minor-mode)
  :config
  (yas-load-directory "~/.dotfiles/.emacs.d/custom-snippets/")
  (yas-reload-all))

(global-set-key (kbd "C-<tab>") 'yas-expand)

(use-package yasnippet-snippets)

;; Lsp
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp
  :hook (treemacs-mode . (display-line-numbers-mode nil)))

(use-package lsp-ivy
  :after lsp)

;; Complétion
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :after lsp)

(use-package helm-lsp
  :after lsp)

(use-package helm
  :after lsp
  :config (helm-mode))

;; C -------------------------------------------------------------------

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

;; Python --------------------------------------------------------------

(use-package python-mode
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (lsp-pyls-server-command "/home/tristan/.local/bin/pyls"))

;; LaTeX ---------------------------------------------------------------

(use-package lsp-latex
  :hook ((latex-mode tex-mode) .
	 (lambda () (require 'lsp-latex) (lsp))))

;; Bash ----------------------------------------------------------------

(add-hook 'sh-mode-hook 'lsp)

;; Java ----------------------------------------------------------------

(use-package lsp-java
  :hook (java-mode . lsp-deferred))

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

;; Terminal ------------------------------------------------------------

;; eshell
(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

;; vterm
(use-package vterm)
(define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
(define-key vterm-mode-map (kbd "C-M-j") #'counsel-switch-buffer)

;; Magit - package qui permet d'utiliser git plus efficacement

(use-package magit)

(use-package magit-todos)

;; Afficher le temps de lancement --------------------------------------

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs (GUI) lancé en  %s."
                     (format "%.2f secondes"
			     (float-time
			      (time-subtract after-init-time before-init-time))))))

(defun tr/display-client-startup-time ()
  (defvar before-frame-time)
  (add-hook 'before-make-frame-hooks '(setq before-frame-time (current-time)))
  (add-hook 'after-make-frame-hooks
            (lambda (frame)
              (with-selected-frame frame
                (message "Emacs (client) lancé en %s."
                     (format "%.2f secondes"
                             (float-time
                              (time-subtract
                               (current-time) before-frame-time))))))))

(defun tr/emacs-client-setup ()
  (tr/emacs-client-frame-setup)
  (tr/display-client-startup-time))

(defun tr/emacs-gui-setup ()
  (tr/emacs-gui-frame-setup))

(if (daemonp)
    (tr/emacs-client-setup)
  (tr/emacs-gui-setup))
