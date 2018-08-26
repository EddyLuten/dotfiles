;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Require common lisp functionality
;;
(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SERVER
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PACKAGE MANAGEMENT
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
;; Set up additional package repositories
;; Commenting out marmalade for the time being since it's fairly broken
;; (add-to-list 'package-archives
;;  '("marmalade" . "https://marmalade-repo.org/packages/"))
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
    projectile
    helm
    wc-mode
    olivetti
    exec-path-from-shell
    htmlize
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
;; ORG-MODE CONFIG
;;
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
    ("macronu" "\\={u}" nil "&#363;" "u" "u" "u")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Markdown-Mode
;;
(use-package markdown-mode
  ;; Set it visually similar to org-mode
  :init
  (add-hook 'markdown-mode-hook 'turn-on-olivetti-mode)
  (add-hook 'markdown-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'markdown-mode-hook (lambda () (wc-mode)))
  :config
  (setq olivetti-body-width 80))

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
;; Global Shortcuts
;;
(global-set-key (kbd "C-c C-e") 'eval-buffer)

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
;; Helm
;;
(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

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
;; UI
;;
(set-face-attribute 'default nil :font "PT Mono-14")

;; Show whitespace
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9])))
(global-whitespace-mode 1)

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
(defun dev-window-setup ()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KEY MAPPING
;;

(setq mac-command-modifier 'meta) ;; map CMD key to meta on macOS
;; Instead of mapping the super to meta key and vice versa on Linux,
;; Set the dip switches on the HHKB to be like so (1-6): 101010

(global-set-key (kbd "C-\\") 'el-next-window)
(global-set-key (kbd "M-\\") 'el-prev-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TABS
;;
(setq default-tab-width 2)
(setq custom-tab-width 2)

(defun infer-indentation-style ()
  ;; Borrowed from emacswiki.org/emacs/NoTabs
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(infer-indentation-style)
(electric-indent-mode +1)
(setq backward-delete-char-untabify-method 'hungry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fullscreen
;; Emulate the TTY as much as possible, no GUI stuff.
;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-maximized)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; I want my damn home and end keys to work on mac
;;
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Boilerplate stuff
;;
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (org))))

