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
;;	     '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; Initialize the package manager
(package-initialize)
(package-refresh-contents)

;; Define the packages that need to be loaded at the start
(defvar my-packages
  '(org
    org-bullets
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
;; put the backups in the ~/saves directory instead
;;
(setq backup-directory-alist `(("." . "~/saves")))
(setq backup-by-copying t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UI
;;
;; I like this font on mac
(if (eq system-type 'darwin)
    (set-face-attribute 'default nil :font "PT Mono-18" ))
(if (eq system-type 'gnu/linux)
    (set-face-attribute 'default nil :font "Latin Modern Mono-14"))
;; I don't want to use the Command key on mac, map it to Meta
(setq mac-command-modifier 'meta)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Fullscreen
;; Emulate the TTY as much as possible, no GUI stuff.
;;
(toggle-frame-fullscreen)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 
