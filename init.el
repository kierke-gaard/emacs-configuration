(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves")) ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(column-number-mode)
(blink-cursor-mode -1)
(transient-mark-mode -1)
(setq indent-tabs-mode nil)
(setq default-tab-width 4)
(display-time-mode t)
(show-paren-mode t)
(setq display-time-24hr-format 1)
(setq-default fill-column 80)
(display-battery-mode t)
(setq inhibit-startup-message t)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq-default whitespace-line-column 80)

(setq auto-save-file-name-transforms
          `((".*" ,(concat user-emacs-directory "auto-save/") t)))

(require 'helm)
(require 'helm-projectile)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq helm-split-window-in-side-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp
      helm-scroll-amount 8
      helm-ff-file-name-history-use-recentf t)
(helm-mode 1)
(helm-projectile-on)

(recentf-mode 1)
(setq recentf-max-saved-items 50)
(global-set-key (kbd "C-x C-y") 'helm-recentf)

(projectile-global-mode)

(use-package treemacs
    :ensure t
    :defer t
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
    :config
    (progn
      (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
            treemacs-deferred-git-apply-delay   0.5
            treemacs-display-in-side-window     t
            treemacs-file-event-delay           5000
            treemacs-file-follow-delay          0.2
            treemacs-follow-after-init          t
            treemacs-follow-recenter-distance   0.1
            treemacs-git-command-pipe           ""
            treemacs-goto-tag-strategy          'refetch-index
            treemacs-indentation                2
            treemacs-indentation-string         " "
            treemacs-is-never-other-window      nil
            treemacs-max-git-entries            5000
            treemacs-no-png-images              nil
            treemacs-project-follow-cleanup     nil
            treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
            treemacs-recenter-after-file-follow nil
            treemacs-recenter-after-tag-follow  nil
            treemacs-show-cursor                nil
            treemacs-show-hidden-files          t
            treemacs-silent-filewatch           nil
            treemacs-silent-refresh             nil
            treemacs-sorting                    'alphabetic-desc
            treemacs-space-between-root-nodes   t
            treemacs-tag-follow-cleanup         t
            treemacs-tag-follow-delay           1.5
            treemacs-width                      40)

      ;; The default width and height of the icons is 22 pixels. If you are
      ;; using a Hi-DPI display, uncomment this to double the icon size.
      ;;(treemacs-resize-icons 44)

      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-fringe-indicator-mode t)
      (pcase (cons (not (null (executable-find "git")))
                   (not (null (executable-find "python3"))))
	(`(t . t)
	 (treemacs-git-mode 'deferred))
	(`(t . _)
	 (treemacs-git-mode 'simple))))
    :bind
    (:map global-map
	  ([f8] . treemacs)
          ("M-0"       . treemacs-select-window)
          ("C-x t 1"   . treemacs-delete-other-windows)
          ("C-x t t"   . treemacs)
          ("C-x t B"   . treemacs-bookmark)
          ("C-x t C-t" . treemacs-find-file)
          ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

(setq cider-eldoc-display-for-symbol-at-point t)
(global-set-key (kbd "C-c M-b") 'cider-repl-clear-buffer)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)
(global-set-key (kbd "C-x r e") 'string-insert-rectangle)

(global-set-key (kbd "C-x g") 'magit-status)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-background nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(csv-separators (quote (";" "	")))
 '(custom-enabled-themes (quote (deeper-blue)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-autodoc elpy-module-sane-defaults)))
 '(package-selected-packages
   (quote
    (csv-mode pyenv-mode jedi fsharp-mode tuareg treemacs-icons-dired treemacs-magit matlab-mode ein blacken scala-mode treemacs-evil elpy ace-window julia-mode switch-window cider-hydra clj-refactor company cider helm helm-ag helm-projectile magit paredit paredit-everywhere projectile treemacs treemacs-projectile use-package)))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(set-face-attribute 'default nil :height 120)
