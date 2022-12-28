;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

(setq inhibit-startup-message t) ;; Enlever l'écran d'accueil
(scroll-bar-mode -1)  ;; Enlever la scrollbar
(tool-bar-mode -1)    ;; Enlever la barre d'outils
(tooltip-mode -1)     ;; Enlever les tooltips
(menu-bar-mode -1)    ;; Enlever la barre de menu
(save-place-mode 1)   ;; Mémoriser le dernier emplacement du curseur

;; Raccourcis clavier
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "M-C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "M-C-w") 'other-window)
(global-set-key (kbd "M-C-l") 'load-theme)
(global-set-key (kbd "M-C-v") 'eval-region)
(global-set-key (kbd "M-C-m") 'vterm)
(global-set-key (kbd "M-C-g a") 'org-agenda)
(global-set-key (kbd "M-C-g c") 'org-capture)

(global-set-key (kbd "C-p") 'kill-whole-line)
(setq kill-whole-line t)

(global-set-key (kbd "C-x K") 'kill-current-buffer)
(global-set-key (kbd "C-$") 'delete-other-windows)

;; Mettre automatiquement les parenthèses fermantes
(electric-pair-mode 1)

;; Activer les numéros de ligne pour les modes où c'est nécessaire
(column-number-mode t)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
		treemacs-mode-hook
		vterm-mode-hook
		help-mode-hook
		helpful-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode nil))))

;; Initialisation des packages
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
