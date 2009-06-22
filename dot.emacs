;; dot.emacs
;; load-path
(setq load-path
      (append (list	       
	       "~/.emacs.d/site-lisp"
;	       "~/.emacs.d/slime"
	       "~/.emacs.d/yatex")
	      load-path))

;;yatex
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

;; for net install
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/site-lisp")

;display
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(show-paren-mode)

(setq default-frame-alist
      (append (list '(font . "M+2VM+IPAG circle")
		    '(fullscreen . fullwidth)
		    '(display-type . "color")
		    '(mouse-color . "black")
		    '(border-color . "black")
		    '(alpha . (80 80))
;		    '(foreground-color . "snow")
;		    '(background-color . "black")
		    '(cursor-color . "green"))
	      default-frame-alist))

;変数定義
(setq truncate-partial-width-windows nil)
(setq-default transient-mark-mode t)
(setq inhibit-startup-message t)

;マッピング
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-m" 'reindent-then-newline-and-indent)

;; for gosh
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8)
	    process-coding-system-alist))
;;; path gosh
(setq gosh-program-name "/usr/local/bin/gosh -i")
;;; use cmuscheme
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process" t)
;;; gosh-mode window
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
    (get-buffer-create "*scheme*"))
  (run-scheme gosh-program-name))


;; ess
(load "~/.emacs.d/ess/lisp/ess-site")
(require 'ess-site)
(defun ess:format-window-1 ()
  (split-window-horizontally)
  (other-window 1)
  (split-window)
  (other-window 1))
(add-hook 'ess-pre-run-hook 'ess:format-window-1)

;; slime
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup)
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))

;; for viper
(setq viper-mode t)
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '3)
(setq vipre-ex-style-editing nil)
(require 'viper)
(require 'vimpulse)
(define-key viper-insert-global-user-map "\C-c" 'viper-exit-insert-state)

(defun my-viper-beginning-of-buffer ()
  (interactive)
  (beginning-of-buffer))
(define-key viper-vi-global-user-map [?g?g] 'my-viper-beginning-of-buffer)
(defun my-viper-star ()
  (interactive)
  (let ((wd (concat "\\<" (thing-at-point 'symbol) "\\>")))
    (setq viper-s-string wd)
    (setq viper-s-forward t)
    (viper-search wd t 1)))
(define-key viper-vi-global-user-map [?*] 'my-viper-star)
(defun my-viper-jump-tag ()
  (interactive)
  (setq wd (thing-at-point 'symbol))
  (find-tag wd))
(define-key viper-vi-global-user-map [?\C-\]] 'my-viper-jump-tag)
(defun my-viper-jump-tag-next ()
  (interactive)
  (setq wd (thing-at-point 'symbol))
  (find-tag wd 0))
(define-key viper-vi-global-user-map [?\C-:] 'my-viper-jump-tag-next)
(defun my-viper-pop-tag ()
  (interactive)
  (pop-tag-mark))
(define-key viper-vi-global-user-map [?\C-t] 'my-viper-pop-tag)
(defun my-viper-pop-mark ()
  (interactive)
  (set-mark-command -1))
(define-key viper-vi-global-user-map [?\C-o] 'my-viper-pop-mark)
(define-key viper-vi-global-user-map [?u] 'undo)
(define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify)
(define-key viper-insert-global-user-map [delete] 'delete-char)
(define-key viper-emacs-global-user-map "\C-w\C-w" 'other-window)
(define-key viper-vi-global-user-map "\C-w\C-w" 'other-window)
(define-key viper-emacs-global-user-map "\C-w\C-o" 'delete-other-windows)
(define-key viper-vi-global-user-map "\C-w\C-o" 'delete-other-windows)  ;; dired を少しだけ vi 風に
(define-key viper-dired-modifier-map "j" 'dired-next-line)
(define-key viper-dired-modifier-map "k" 'dired-previous-line)
(define-key viper-dired-modifier-map "/" 'dired-goto-file)
(define-key viper-dired-modifier-map "l" '(lambda () (interactive) (dired-next-line 10)))
(define-key viper-dired-modifier-map "h" '(lambda () (interactive) (dired-previous-line 10)))
(define-key viper-insert-basic-map "\C-[" 'viper-intercept-ESC-key)

;; dabbrev-expand-multiple
(require 'dabbrev-expand-multiple)
(global-set-key "\M-/" 'dabbrev-expand-multiple)

;; w3m-emacs
(require 'w3m-load)
(setq w3m-default-display-inline-images t)


;;eshell
(setq eshell-glob-include-dot-dot nil)

;; uim.el
(require 'uim)
;(autoload 'uim-mode "uim" nil t)
(global-set-key "\C-j" 'uim-mode)


(require 'jaspace)
