;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Require common lisp functionality
;;
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SERVER
;;
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PACKAGE MANAGEMENT
;;
(require 'package)
;; Set up additional package repositories
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; Initialize the package manager
(package-initialize)
(package-refresh-contents)

;; Define the packages that need to be loaded at the start
(defvar my-packages
  '(org
    org-bullets
    markdown-mode
    move-text
    speedbar
    sr-speedbar
    projectile
    projectile-speedbar
    helm
    wc-mode
    olivetti
    color-theme-sanityinc-tomorrow
    exec-path-from-shell
    htmlize
    editorconfig
    use-package
    restart-emacs)
  "A list of packages to ensure are installed at launch.")

;; Automatically install missing packages
(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))); fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; use-package allows for easier package configuration
;;
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EditorConfig <3
;;
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ORG-MODE CONFIG
;;
(require 'org)
(require 'org-bullets)
(use-package org
     :init
     (add-hook 'org-mode-hook 'turn-on-olivetti-mode)
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
     (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
     (add-hook 'org-mode-hook (lambda () (wc-mode)))
     :config
     (setq olivetti-body-width 80))

;; Add special characters to the list, the order is:
;; ("replacement" "latex" nil "html" "ascii" "latin1" "utf8")
(defvar my-replacements
  '(("schwa" "\\textschwa{}" nil "&#601;" "e" "e" "e")
    ("openo" "\\textopeno{}" nil "&#596;" "o" "o" "o")
    ("scr" "\\textscr{}" nil "&#640;" "R" "R" "R")
    ("sci" "\\textsci{}" nil "&#618;" "I" "I" "I")
    ("turnv" "\\textturnv{}" nil "&#652;" "u" "u" "u")
    ("macrona" "\\={a}" nil "&#257;" "a" "a" "a")
    ("macrone" "\\={e}" nil "&#275;" "e" "e" "e")
    ("macroni" "\\={i}" nil "&#299;" "i" "i" "i")
    ("macrono" "\\={o}" nil "&#332;" "o" "o" "o")
    ("macronu" "\\={u}" nil "&#363;" "u" "u" "u")
    ("scene" "\\vspace{\\baselineskip}" nil "<br/><br/>" "\\n\\n" "\\n\\n")
    ("scenebreak" "\\vspace{\\baselineskip}" nil "<br/><br/>" "\\n\\n" "\\n\\n")))

(dolist (repl my-replacements)
  (add-to-list 'org-entities-user repl))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
    '("el-book"
      "\\documentclass{book}"
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq org-todo-keyword-faces
      '(("TODO" . "red")
        ("DOING" . "yellow")
        ("DONE" . (:foreground "green" :weight bold))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Markdown-Mode
;;
(use-package markdown-mode
  ;; Set it up visually similar to org-mode
  :init
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)
  (add-hook 'markdown-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'markdown-mode-hook (lambda () (wc-mode)))
  :config
  (setq olivetti-body-width 80)
  (setq markdown-command "/usr/bin/pandoc"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Term-mode
;;
(add-hook 'term-mode-hook (lambda () (display-line-numbers-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Use User-$PATH
;;
(exec-path-from-shell-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; I don't want to type out "yes" or "no"
;;
(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Global Keys
;;
(global-set-key (kbd "C-c C-e") 'eval-buffer)
(global-set-key (kbd "<f2>") 'el-sr-speedbar-toggle)
(global-set-key (kbd "C-<f2>") 'projectile-speedbar-open-current-buffer-in-tree)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "<home>") 'beginning-of-line) ;; for macos
(global-set-key (kbd "<end>") 'end-of-line) ;; for macos
(global-set-key (kbd "C-\\") 'el-next-window)
(global-set-key (kbd "M-\\") 'el-prev-window)

(setq mac-command-modifier 'meta) ;; map CMD key to meta on macOS
;; Instead of mapping the super to meta key and vice versa on Linux,
;; Set the dip switches on the HHKB to be like so (1-6): 101010
;; Also, on Ubuntu, download the Tweaks application and modify the left
;; alt and left Super keys to be swapped if not using a HHKB.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Projectile
;;
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-project-search-path '("~/code/" "~/org/"))

;; Don't contact the remote SVN server for searches
(setq projectile-svn-command
      "find . -type f -not -iwholename '*.svn/*' -print0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Speedbar / sr-speedbar
;;
(require 'sr-speedbar)
(setq speedbar-show-unknown-files t)
(setq speedbar-directory-unshown-regexp
      "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")
(setq sr-speedbar-right-side nil)

;; Keeps the focus in the editing buffer, not in the speedbar
(defun el-sr-speedbar-toggle ()
  (interactive)
  (if (sr-speedbar-exist-p)
      (sr-speedbar-close)
    (projectile-speedbar-open-current-buffer-in-tree)))

(add-hook 'speedbar-mode-hook
          (lambda () (display-line-numbers-mode -1)))
(add-hook 'speedbar-before-visiting-file-hook
          (lambda () (other-window +1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helm
;;
(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Misc. Package Config
;;

;; Move text with M-<up> and M-<down>
(move-text-default-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; put the backups in the ~/saves directory instead
;;
(setq backup-directory-alist `(("." . "~/saves")))
(setq backup-by-copying t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TABS
;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq custom-tab-width 2)
(setq backward-delete-char-untabify-method 'hungry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UI
;;
(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 140)

;; TODO find a good way to display trailing and indent whitespace

;; Show line numbers
(global-display-line-numbers-mode +1)

;; Define some shortcuts for splitting windows
(defun el-next-window ()
  (interactive)
  (other-window +1))

(defun el-prev-window ()
  (interactive)
  (other-window -1))

;; Split windows in a predetermined way for development.
(defun el-dev-window-setup ()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  ;; open whatever is the next buffer in this window
  (el-next-window)
  (split-window-below)
  (switch-to-next-buffer)
  ;; open the terminal and go back to the main window
  (el-next-window)
  (term "/usr/bin/zsh")
  (display-line-numbers-mode -1)
  (el-next-window))

;; This isn't always a good idea, but commenting this out in case it
;; becomes a good idea again.
;; (add-hook 'window-setup-hook 'default-window-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Hide the UI stuff and start in maximized mode
;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-maximized)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Startup behavior
;;
(add-hook 'window-setup-hook
          ;; Show the most recent files when starting up emacs
          (lambda () (setq initial-buffer-choice (helm-recentf))))

(setq inhibit-startup-screen t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Boilerplate stuff
;;
(when (memq window-system '(mac ns  on loaded hook))
  (exec-path-from-shell-initialize))

(load-theme 'sanityinc-tomorrow-bright t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-tomorrow projectile-speedbar sr-speedbar wc-mode use-package restart-emacs projectile org-bullets olivetti move-text markdown-mode htmlize helm exec-path-from-shell editorconfig))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
