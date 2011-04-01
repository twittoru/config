;; dot.emacs

;;macros{{{
;;http://www.sodan.org/~knagano/emacs/dotemacs.html
(defmacro exec-if-bound (sexplist)
  "関数が存在する時だけ実行する。（car の fboundp を調べるだけ）"
  `(if (fboundp (car ',sexplist))
       ,sexplist))
(defmacro defun-add-hook (hookname &rest sexplist)
  "add-hook のエイリアス。引数を関数にパックして hook に追加する。"
  `(add-hook ,hookname
             (function (lambda () ,@sexplist))))
(defun load-safe (loadlib)
  "安全な load。読み込みに失敗してもそこで止まらない。"
  ;; missing-ok で読んでみて、ダメならこっそり message でも出しておく
  (let ((load-status (load loadlib t)))
    (or load-status
        (message (format "[load-safe] failed %s" loadlib)))
    load-status))
(defmacro eval-safe (&rest body)
  "安全な評価。評価に失敗してもそこで止まらない。"
  `(condition-case err
       (progn ,@body)
     (error (message "[eval-safe] %s" err))))
(defun autoload-if-found (function file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (and (locate-library file)
       (autoload function file docstring interactive type)))
;;http://e-arrows.sakura.ne.jp/2010/03/macros-in-emacs-el.html
(defmacro req (lib &rest body)
  `(when (locate-library ,(symbol-name lib))
     (require ',lib) ,@body))
;;}}}
;;{{{load-path 
(require 'cl)
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)
(setenv "LANG"  "ja_JP.UTF-8")
(setq load-path
      (append (list       
                "~/.emacs.d/site-lisp/"
                "~/local/clbuild/source/slime/"
                "/opt/local/share/emacs/site-lisp/apel/"
                "/opt/local/share/emacs/23.1/site-lisp/skk/"
                "/opt/local/share/emacs/23.1/site-lisp/emu/"
                "~/.emacs.d/emacs-tiny-tools/lisp/other/"
                "~/.emacs.d/swank-clojure/"
                "~/.emacs.d/swank-gauche/"
                "~/.emacs.d/clojure-mode/"
                "~/.emacs.d/yatex/"
                "~/.emacs.d/ess/lisp/"
                "~/.emacs.d/navi2ch/"
                "~/.emacs.d/vimpulse/"
                "~/local/repo/emacs-w3m/"
                )
              load-path))
(setq exec-path
      (append (list
                "/opt/local/bin"
                "/opt/local/sbin"
                "/usr/local/bin"
                "/usr/local/sbin"
                "/Users/tor/local/bin"
                "/usr/bin"
                "/bin"
                "/usr/sbin"
                "/sbin"
                )
              exec-path))
(setenv "PATH" "/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:/Users/tor/local/bin:/usr/bin:/bin:/usr/sbin:/sbin") 
;;}}}
;;{{{yatex
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload-if-found  'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;;}}}
;;{{{imaxima
(autoload-if-found 'imaxima "imaxima" "Frontend of Maxima CAS" t)
(autoload-if-found 'imath "imath" "Interactive Math mode" t)
(autoload-if-found 'imath-mode "imath" "Interactive Math mode" t)
;;}}}
;;{{{auto-install
;(eval-safe (require 'auto-install))
;(setq auto-install-directory "~/.emacs.d/site-lisp/")
;(auto-install-update-emacswiki-package-name t)
;(auto-install-compatibility-setup)
;;}}}
;;{{{gosh
(setq prooding-system-alist
      (cons '("gosh" utf-8 . utf-8)
            process-coding-system-alist)
      gosh-program-name "/usr/local/bin/gosh -i")
(autoload-if-found 'scheme-mode "cmuscheme" "Major mode for Scheme" t)
(autoload-if-found 'run-scheme "cmuscheme" "Run an inferior Scheme process" t)
;;}}}
;;{{{slime
(eval-after-load "slime"
                 '(progn
                    (slime-setup '(
                                   slime-repl
                                   slime-fuzzy
                                   slime-banner
                                   slime-c-p-c
                                   ))))
(setq slime-net-coding-system 'utf-8-unix)
(defun-add-hook 'lisp-mode-hook
  (cond ((not (featurep 'slime))
         (require 'slime) 
         (normal-mode))))
;(require 'slime)
;;}}}
;;{{{ess
(setq auto-mode-alist
      (cons (cons "\\.R$" 'R-mode) auto-mode-alist))
(autoload-if-found 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
(autoload-if-found 'R "ess-site" nil 'interactive)
(setq ess-ask-for-ess-directory nil)
(setq ess-pre-run-hook
      '((lambda ()
          (setq default-process-coding-system '(utf-8 . utf-8))
          ;; iess-mode keymapping for viper
          (define-key inferior-ess-mode-map [return] 'inferior-ess-send-input)
          )))
(eval-safe (require 'align))
(add-to-list 'align-rules-list
             '(ess-assignment-operator
                (regexp . "\\(\\s-*\\)<-[^#\t\n]")
                (repeat . nil)
                (modes  . '(ess-mode))))

;;}}}
;;{{{viper

(setq viper-mode t)
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '3)
;;; set autoindent shiftwidth=4
(setq viper-auto-indent t) 
(setq viper-shift-width 4)

(setq ex-cycle-other-window nil) 
;;; f,F,t,T はcursorがある行にのみ働く。
(setq viper-allow-multiline-replace-regions nil)

(eval-safe
  (require 'redo)
  (require 'viper)
  (require 'vimpulse)
  (define-key vimpulse-visual-global-user-map [?j]  'next-line)
  (define-key vimpulse-visual-global-user-map [?k]  'previous-line))


(defadvice viper-change-cursor-color
           ;;チラツキ解消 -- 発現していないが導入しておく
           ;;http://d.hatena.ne.jp/leque/20100304/p2
           (around kludge-to-avoid-cursor-flicker-on-Cocoa-Emacs activate)
           nil)
;;}}}
;;{{{w3m-emacs

(eval-safe (require 'w3m-load))
(setq w3m-default-display-inline-images t)

;;}}}
;;{{{jaspace

(eval-safe (require 'jaspace))

;;}}}
;;{{{eshell

(setq eshell-glob-include-dot-dot nil)
;;; http://www.emacswiki.org/emacs/EshellAndViper
(defun-add-hook 'eshell-mode-hook
  (when viper-mode
    (setq viper-auto-indent nil)))

;;}}}
;;{{{uniquify

;;;buffer name modify
(eval-safe (require 'uniquify))
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;}}}
;;{{{migemo

(setq migemo-command "/usr/local/bin/cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\a"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1024)
(setq migemo-isearch-min-length 2)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
;;; initialization
(migemo-init)

;;}}}
;;{{{linum -- line number

(eval-safe
  (require 'linum)
  (exec-if-bound (global-linum-mode t))
  (setq linum-format "%4d"))

;;}}}
;;{{{folding -- vimlike fold
(load-safe "folding")
(eval-safe
  (folding-mode-add-find-file-hook)
  (folding-add-to-marks-list 'emacs-lisp-mode ";;{{{" ";;}}}" nil t))
;;}}}
;;{{{skk

(eval-safe
  (require 'skk-autoloads)
  (setq-default skk-kutoten-type 'en))
(setq skk-kuten-touten-alist
      '(
        (jp . ("。" . "、"))
        (en . ("." . ","))
        ))
(setq skk-isearch-start-mode 'latin)
(setq skk-large-jisyo nil)
(setq skk-server-host "127.0.0.1")
(setq skk-server-portnum 1178)
(setq skk-show-annotation t)

;;}}}
;;{{{iswitchb

;;; http://www.bookshelf.jp/soft/meadow_28.html
(iswitchb-mode 1)
(iswitchb-default-keybindings)

(setq iswitchb-regexp t)
(setq iswitchb-use-migemo-p t)
(defadvice iswitchb-get-matched-buffers
           (before iswitchb-use-migemo activate)
           "iswitchb で migemo を使ってみる。"
           (when iswitchb-use-migemo-p
             (ad-set-arg
               0 (migemo-get-pattern
                   (ad-get-arg 0)))))

;;}}}
;;{{{undo-tree
(eval-safe
  (require 'undo-tree)
  (define-key undo-tree-visualizer-map [?k] 'undo-tree-visualize-undo) 
  (define-key undo-tree-visualizer-map [?j] 'undo-tree-visualize-redo) 
  (define-key undo-tree-visualizer-map [?h] 'undo-tree-visualize-switch-branch-left)
  (define-key undo-tree-visualizer-map [?l] 'undo-tree-visualize-switch-branch-right)
  (define-key undo-tree-visualizer-map [?q] 'undo-tree-visualizer-quit) 
  (global-undo-tree-mode))
; auto-set insert-mode to use vi-keybind in visualizer-mode
; undo-tree has not any hook ><
(defadvice undo-tree-visualizer-mode
           (after viper-undo-tree-visualizer-mode activate)
           (viper-insert t))


;;}}}
;;{{{navi2ch
(autoload-if-found 'navi2ch' "navi2ch" "Navigator for 2ch for Emacs" t)
;;}}}
;;{{{misc

;font from http://sakito.jp/emacs/emacs23.html
(when (>= emacs-major-version 23)
  (eval-safe
    (if (not window-system)
      (tool-bar-mode 0)
      (menu-bar-mode 0)
      (scroll-bar-mode 0)
      (display-time-mode 0))
    (set-face-attribute 'default nil
                        :family "monaco"
                        :height 140)
    (set-fontset-font
      (frame-parameter nil 'font)
      'japanese-jisx0208
      '("Hiragino Maru Gothic Pro" . "iso10646-1"))
    (set-fontset-font
      (frame-parameter nil 'font)
      'japanese-jisx0212
      '("Hiragino Maru Gothic Pro" . "iso10646-1"))
    (set-fontset-font
      (frame-parameter nil 'font)
      'mule-unicode-0100-24ff
      '("monaco" . "iso10646-1"))
    (setq face-font-rescale-alist
          '(("^-apple-hiragino.*" . 1.1)
            (".*osaka-bold.*" . 1.1)
            (".*osaka-medium.*" . 1.2)
            (".*courier-bold-.*-mac-roman" . 1.0)
            (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
            (".*monaco-bold-.*-mac-roman" . 0.9)
            ("-cdac$" . 1.3)))))

(setq default-frame-alist
      (append (list 
                '(display-type . "color")
                '(mouse-color . "black")
                '(border-color . "black")
                '(alpha . (80 50))
                '(foreground-color . "snow")
                '(background-color . "black")
                '(cursor-color . "green"))
              default-frame-alist))

;変数定義
(setq-default transient-mark-mode t)
(setq inhibit-startup-message t)

;マッピング
(global-set-key "\C-h" 'backward-delete-char-untabify)
(global-set-key "\C-m" 'reindent-then-newline-and-indent)

(setq make-backup-files t)       ; バックアップファイルを作成する。
;;; バックアップファイルの保存場所を指定。
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "~/.backup/"))
            backup-directory-alist))
(setq temporary-file-directory "~/.backup/tmp/")

(setq version-control t)     ; 複数のバックアップを残します。世代。
(setq kept-new-versions 5)   ; 新しいものをいくつ残すか
(setq kept-old-versions 5)   ; 古いものをいくつ残すか
(setq delete-old-versions t) ; 確認せずに古いものを消す。
(setq vc-make-backup-files t) ; バージョン管理下のファイルもバックアップを作る。
;shebangが付いているファイルのパーミッションを保存時に +x にしてくれる設定
(defun-add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq
  ;; beep を止めて、flash screen にする。
  visible-bell t
  ring-bell-function 'ignore
  ;; Find-File 等で、ignore-case な補完
  completion-ignore-case t
  read-file-name-completion-ignore-case t
  ;; emacs が file.~version~ のようなファイルを作らなくなる。
  ;; emacs から cvs を使う場合は、このファイルがあると動作が高速。
  vc-cvs-stay-local nil
  ;; スクロールする行数を1にする
  scroll-conservatively 100
  ;; garbage collectionの頻度を減らして、速度向上
  ;; デフォルトは400000
  gc-cons-threshold 1000000
  ;; 縦分割画面で、長い行をwrapさせる。
  truncate-partial-width-windows nil
  ;; hoge.txt~のようなバックアップファイルを作らない
  backup-inhibited t
  )

;;
;;;; yes or not を y or n にする
(fset 'yes-or-no-p 'y-or-n-p)
;;
;;;; 画像ファイルを表示
;(auto-image-file-mode t)
;;
;;;; ファイルを開いた時に以前いた行に移動する
(setq-default save-place t)
(setq-default save-place-file "~/.emacs.d/save-place")
(eval-safe (require 'saveplace) )
;;
;;

(blink-cursor-mode 0)
;;
;; 行をハイライト
(setq hl-line-face 'underline)
(global-hl-line-mode)
;
;;;;; ハイライトされていなくても、選択範囲が存在する
(setq mark-even-if-inactive t)
;;; ペーストする時に選択範囲があれば、それを上書き
(delete-selection-mode 1)

;;;;; http://valvallow.blogspot.com/2010/06/emacs_02.html
;;;;; Emacs 行末のスペースに色をつける
(when (boundp 'show-trailing-whitespace)
    (setq-default show-trailing-whitespace t))
(set-face-background 'trailing-whitespace "purple4")

;;}}}
;;; -*- emacs-lisp -*-
;;; .emacs 21 for euske
;
;(set-language-environment "Japanese")
;(global-font-lock-mode t)
;(cd "~/")
;
;;;  display
;;;
;(defun display-to-screen (s) (interactive "sString: ")
;  (if (and (not window-system) (string-equal "screen" (getenv "TERM")))
;      (send-string-to-terminal (concat "\033k" s "\033\134"))))
;(if window-system
;    (progn
;      (setq visible-bell t)
;      (server-start)
;      (menu-bar-mode -1)
;      (tool-bar-mode -1)
;      (scroll-bar-mode -1))
;  (progn
;    (setq visible-bell nil)
;    (set-terminal-coding-system 'euc-jp)
;    (menu-bar-mode -1)
;    (defun display-window-title () (interactive) (display-to-screen "Emacs"))
;    (add-hook 'suspend-resume-hook (function display-window-title))
;    (display-window-title)
;    (set-face-foreground 'mode-line "gold")
;    (set-face-background 'mode-line "black")
;    ))
;
;
;;;  general
;;;
;(put 'eval-expression 'disabled nil)
;(put 'erase-buffer 'disabled nil)
;(setq-default fill-column 66)
;(put 'downcase-region 'disabled nil)
;(prefer-coding-system 'euc-jp)
;(setq next-line-add-newlines nil
;      inhibit-startup-message t
;      require-final-newline  t
;      auto-save-list-file-prefix nil
;      suggest-key-bindings   nil
;      dired-listing-switches "-ao"
;;      scroll-conservatively  1
;      default-buffer-file-coding-system 'euc-jp
;      make-backup-files      nil
;      )
;
;
;;;  find-exist-file
;;;
;(defun find-exist-file (fname) (interactive "fFind exist file: ")
;  (if (car (file-attributes
;	    (file-chase-links (expand-file-name fname))))
;      (dired fname)
;    (find-file fname)))
;(global-set-key "\C-x\C-f" 'find-exist-file)
;(global-set-key "\C-xF"    'find-file)
;
;(defun add-load-path (d) (interactive "DLoad path: ")
;  (if (not (member d load-path))
;      (setq load-path (cons d load-path))))
;
;
;;;  display-time
;;;
;(setq display-time-24hr-format t)
;(display-time)
;(line-number-mode 1)
;(column-number-mode 1)
;
;
;;;  kanji operation
;;;
;(defun euc () (interactive) (set-buffer-file-coding-system 'euc-jp-unix))
;(defun jis () (interactive) (set-buffer-file-coding-system 'iso-2022-jp-unix))
;(defun sjis () (interactive) (set-buffer-file-coding-system 'sjis-unix))
;(defun dos () (interactive) (set-buffer-file-coding-system 'sjis-dos))
;
;
;;;  skk
;;;
;(defvar skk-isearch-switch t)
;(require 'skk "skk")
;(global-set-key "\C-x\C-j" 'skk-mode)
;
;
;;;  Python-mode
;;;
;(add-load-path "/usr/share/emacs/site-lisp/python-mode/")
;(require 'python-mode)
;(add-hook 'python-mode-hook
;	  (function (lambda ()
;		      (if (zerop (buffer-size))
;			  (insert-file "~/lib/python/python-template.py")))))
;
;
;;;  c-mode
;;;
;(setq c-default-style
;      '((java-mode . "java")
;	(other . "cc-mode")
;	))
;(defun c-indent-paragraph () (interactive)
;  (let ((endmark (progn (forward-paragraph 1) (point-marker))))
;    (backward-paragraph)
;    (while (< (point) (marker-position endmark))
;      (c-indent-command) (forward-line 1))))
;
;;;  c++-mode
;;;
;(setq c++-empty-arglist-indent 4
;      c++-friend-offset        0)
;(defun c++-indent-paragraph () (interactive)
;  (let ((endmark (progn (forward-paragraph 1) (point-marker))))
;    (backward-paragraph)
;    (while (< (point) (marker-position endmark))
;      (c++-indent-command) (forward-line 1))))
;(defvar c++-mode-map (make-sparse-keymap))
;(define-key c++-mode-map "\M-q" 'c++-indent-paragraph)
;
;
;;;  key bindings
;;;
;(global-unset-key "\C-j")
;(global-unset-key "\C-\]")
;(global-unset-key [insert])
;(global-unset-key [insertchar])
;(global-unset-key "\C-xm")
;(global-unset-key "\C-xt")
;(global-unset-key "\C-\\")
;(global-set-key "\M-r" 'query-replace-regexp)
;(global-set-key "\C-s" 'isearch-forward-regexp)
;(global-set-key "\C-r" 'isearch-backward-regexp)
;(global-set-key "\M-l" 'goto-line)
;(global-set-key "\C-xV" 'set-variable)
;(global-set-key "\M-\C-w" 'kill-ring-save)
;(global-set-key "\C-h\C-a" 'apropos)
;(global-set-key "\C-h\C-v" 'apropos-variable)
;(define-key minibuffer-local-map "\C-p" 'previous-history-element)
;(define-key minibuffer-local-map "\C-n" 'next-history-element)
;(define-key minibuffer-local-completion-map "\C-p" 'previous-history-element)
;(define-key minibuffer-local-completion-map "\C-n" 'next-history-element)
;(define-key minibuffer-local-ns-map "\C-p" 'previous-history-element)
;(define-key minibuffer-local-ns-map "\C-n" 'next-history-element)
;(define-key minibuffer-local-must-match-map "\C-p" 'previous-history-element)
;(define-key minibuffer-local-must-match-map "\C-n" 'next-history-element)
;
;
;;;  C-h <- Delete,  C-t <- C-h
;;;
;(setq key-translation-map (make-sparse-keymap))
;(define-key key-translation-map "\C-t" "\C-h")
;(define-key key-translation-map "\C-h" "\C-\?")
;(global-set-key "\M-h" 'backward-kill-word)
;; for screen
;(define-key key-translation-map "\M-OM" "\C-j")
;(define-key key-translation-map "\M-[A" [up])
;(define-key key-translation-map "\M-[B" [down])
;(define-key key-translation-map "\M-[C" [right])
;(define-key key-translation-map "\M-[D" [left])
;
;
;;;  miscellaneous
;;;
;(defun itelate (f begin end)
;  "Apply a given function at beginning of each lines of the region."
;  (let ((m (set-marker (make-marker) end)))
;    (goto-char begin)
;    (while (< (point) m)
;      (beginning-of-line)
;      (funcall f m)
;      (forward-line 1))))
;
;(defun filter (proc x)
;  (let ((r nil))
;    (dolist (e x r)
;      (if (funcall proc e)
;	  (setq r (cons e r))))))
;
;(defun startswith (s prefix)
;  (and (<= (length prefix) (length s))
;       (string= prefix (substring s 0 (length prefix)))))
;
;; disable ange-ftp
;(setq file-name-handler-alist
;      (filter 
;       (function (lambda (x) (not (startswith (symbol-name (cdr x)) "ange-ftp"))))
;       file-name-handler-alist))
;
;(defun eval-region-message (begin end) (interactive "r")
;  (eval-region begin end) (message "Eval done."))
;(global-set-key "\C-x\C-e" 'eval-region-message)
;
;(defun elmacro (begin end) (interactive "r")
;  (itelate (function (lambda (m) (save-excursion (call-last-kbd-macro))))
;	   begin end))
;
;(defun elinsert (begin end) (interactive "r")
;  (let ((s (read-from-minibuffer "String: ")))
;    (itelate (function (lambda (m) (insert s)))
;	     begin end)))
;
;(defun dediff (begin end) (interactive "r")
;  (itelate (function (lambda (m) 
;	     (if (search-forward-regexp "^\\(\\+ \\|- \\|> \\|< \\)" m t)
;		 (replace-match "" t t))))
;	   begin end))
;
;(defun tab (n) (interactive "nTab-width: ")
;  (setq tab-width n) (message "Tab-width is %d." n))
;
;(defun tmp () (interactive)
;  (switch-to-buffer "*scratch*") (lisp-interaction-mode))
;
;(defun x () (interactive) (insert-file "~/tmp/x"))
;
;(defun scr () (interactive)
;  (set-mark-command nil)
;  (call-process "xscrbuf-j" nil t t))
;(defun url () (interactive)
;  (set-mark-command nil)
;  (call-process "xscrbuf-url" nil t t))
;
;(defun copy-line-as-kill () (interactive)
;  (save-excursion
;    (let ((m (point)))
;      (beginning-of-line)
;      (forward-line 1)
;      (copy-region-as-kill m (point))
;      )))
;(global-set-key "\M-k" 'copy-line-as-kill)
;
;;(setq backup-by-copying t)
;;(fset 'make-backup-file-name
;;      (lambda (file) (concat (expand-file-name "~/.backup/")
;;			     (file-name-nondirectory file))))
;
;
;;;  html
;;;
;
;(defun html-convert-region (x y) (interactive "r")
;  (let ((m (set-marker (make-marker) y)))
;    (save-excursion
;      (goto-char x)
;      (while (search-forward "&" m t) (replace-match "&amp;" t t))
;      (goto-char x)
;     (while (search-forward ">" m t) (replace-match "&gt;" t t))
;      (goto-char x)
;      (while (search-forward "<" m t) (replace-match "&lt;" t t)))))
;
;(defun block (begin end) (interactive "r")
;  (let ((e (set-marker (make-marker) end)))
;    (goto-char begin) (insert-string "<blockquote>\n")
;    (goto-char e) (insert-string "</blockquote>\n")))
;
;(defun pre (begin end) (interactive "r")
;  (let ((e (set-marker (make-marker) end)))
;    (html-convert-region begin e)
;    (goto-char begin) (insert-string "<blockquote><pre>\n")
;    (goto-char e) (insert-string "</pre></blockquote>\n")))
;
;(defun html-insert-tag (begin end tag &optional attrs conv)
;  (let ((e (set-marker (make-marker) end))
;	(a (if (stringp attrs) (concat " " attrs) "")))
;    (if conv (html-convert-region begin end))
;    (goto-char begin) (insert-string (concat "<" tag a ">"))
;    (goto-char e) (insert-string (concat "</" tag ">"))
;    ))
;
;(setq html-mode-map (make-sparse-keymap))
;(define-key html-mode-map "\C-cs" (function (lambda (b e) (interactive "r") (html-insert-tag b e "strong"))))
;(define-key html-mode-map "\C-cr" (function (lambda (b e) (interactive "r") (html-insert-tag b e "span" "class=comment"))))
;(define-key html-mode-map "\C-cu" (function (lambda (b e) (interactive "r") (html-insert-tag b e "u"))))
;(define-key html-mode-map "\C-ce" (function (lambda (b e) (interactive "r") (html-insert-tag b e "em"))))
;(define-key html-mode-map "\C-cc" (function (lambda (b e) (interactive "r") (html-insert-tag b e "code" nil t))))
;(define-key html-mode-map "\C-ck" (function (lambda (b e) (interactive "r") (html-insert-tag b e "kbd" nil t))))
;
;;; timestamps
;(add-hook 'html-mode-hook
;	  (function (lambda () 
;		      (add-hook 'local-write-file-hooks 'html-update-timestamp))))
;(defvar html-helper-timestamp-start "<!-- hhmts start -->\n")
;(defvar html-helper-timestamp-end "<!-- hhmts end -->")
;(defun html-update-timestamp ()
;  "Basic function for updating timestamps. It finds the timestamp in
;the buffer by looking for html-helper-timestamp-start, deletes all text
;up to html-helper-timestamp-end, and runs html-helper-timestamp-hook
;which will presumably insert an appropriate timestamp in the buffer."
;  (save-excursion
;    (goto-char (point-max))
;    (if (not (search-backward html-helper-timestamp-start nil t))
;	(message "timestamp delimiter start was not found")
;      (let ((ts-start (+ (point) (length html-helper-timestamp-start)))
;	    (ts-end (if (search-forward html-helper-timestamp-end nil t)
;			(- (point) (length html-helper-timestamp-end))
;		      nil)))
;	(if (not ts-end)
;	    (message "timestamp delimiter end was not found. Type C-c C-t to insert one.")
;	  (delete-region ts-start ts-end)
;	  (goto-char ts-start)
;	  (run-hooks 'html-helper-timestamp-hook)))))
;  nil)
;(defun my-html-timestamp ()
;  (insert "Last Modified: ")
;  (call-process "env" nil t nil "date"))
;(setq html-helper-timestamp-hook (function my-html-timestamp))
;
;(defun bm () (interactive) 
;  (find-file "~/rc/bm.html"))
;
;(defun memo () (interactive) 
;  (find-file "~/Site/tabesugi.net/memo/cur/cur.html"))
;
;(defun memo1 () (interactive)
;  (call-process "date" nil t nil "+<h5><a href=\"#%d%H%M\" name=\"%d%H%M\">(%H:%M)</a></h5>"))
;
;(defun uuid () (interactive)
;  (call-process "uuid" nil t nil))
;
;(defun newdate () (interactive)
;  (call-process "date" nil t nil "+<h4><a href=\"#%d\" name=\"%d\">/%d</a> <small>[%a]</small></h4>\n<hr noshade size=\"2\">")
;  (memo1))
;
;
;;;  custom
;;;
;(custom-set-variables
;  ;; custom-set-variables was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
; '(browse-url-netscape-program "firefox")
; '(python-honour-comment-indentation nil)
; '(skk-auto-okuri-process nil)
; '(skk-egg-like-newline nil)
; '(skk-rom-kana-rule-list (quote (("hh" "h" ("$B%C(B" . "$B$C(B")) ("mm" "m" ("$B%s(B" . "$B$s(B")) ("nn" "n" ("$B%s(B" . "$B$s(B")) ("?" nil "?") ("@" nil "@") ("$" nil "$") (";" nil ";") (":" nil ":") ("z[" nil "$B!X(B") ("z]" nil "$B!Y(B") ("z{" nil ("$B!Z(B" . "$B!Z(B")) ("z}" nil ("$B![(B" . "$B![(B")) ("z`" nil ("$B!H(B" . "$B!H(B")) ("z'" nil ("$B!I(B" . "$B!I(B")) ("z." nil ("$B!&(B" . "$B!&(B")) ("z:" nil ("$B!D(B" . "$B!D(B")) ("z~" nil ("$B!A(B" . "$B!A(B")) ("z>" nil ("$B"*(B" . "$B"*(B")) ("z<" nil ("$B"+(B" . "$B"+(B")) ("z^" nil ("$B",(B" . "$B",(B")) ("zv" nil ("$B"-(B" . "$B"-(B")) ("z*" nil ("$B"((B" . "$B"((B")) ("z-" nil ("$B!](B" . "$B!](B")) ("z@" nil ("$B!w(B" . "$B!w(B")) ("z/" nil ("$B!?(B" . "$B!?(B")) ("z " nil ("$B!!(B" . "$B!!(B")))))
; '(skk-search-prog-list (quote ((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t) (skk-search-jisyo-file skk-initial-search-jisyo 10000 t) (skk-search-jisyo-file skk-jisyo 0 t) (skk-search-small-dic) (skk-okuri-search) (skk-search-server skk-aux-large-jisyo 10000))))
; '(skk-server-portnum 1178)
; '(skk-share-private-jisyo t))
;
;(custom-set-faces
;  ;; custom-set-faces was added by Custom.
;  ;; If you edit it by hand, you could mess it up, so be careful.
;  ;; Your init file should contain only one such instance.
;  ;; If there is more than one, they won't work right.
; '(font-lock-comment-face ((((type tty pc) (class color) (background dark)) (:foreground "green"))))
; '(font-lock-function-name-face ((((type tty) (class color)) (:foreground "cyan"))))
; '(font-lock-keyword-face ((((type tty) (class color)) (:foreground "yellow"))))
; '(font-lock-string-face ((((type tty) (class color)) (:foreground "plum1"))))
; '(font-lock-type-face ((t (:foreground "cyan"))))
; '(font-lock-variable-name-face ((((type tty) (class color)) (:foreground "orange" :weight light))))
; '(isearch ((((type tty pc) (class color)) (:background "red" :foreground "yellow"))))
; '(lazy-highlight ((((type tty pc) (class color)) (:background "blue")))))

;;http://weboo-returns.com/blog/emacs-shows-double-space-and-tab/
(setq whitespace-style
            '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
            '((space-mark ?\x3000 [?\□])
                      (tab-mark   ?\t   [?\xBB ?\t])))
(eval-safe
  (require 'whitespace)
  (global-whitespace-mode 1)
  (set-face-foreground 'whitespace-space "LightSlateGray")
  (set-face-background 'whitespace-space "DarkSlateGray")
  (set-face-foreground 'whitespace-tab "LightSlateGray")
  (set-face-background 'whitespace-tab "DarkSlateGray"))
