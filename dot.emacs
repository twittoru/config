;; dot.emacs
;; load-path {{{3
(setq load-path
      (append (list	      $
	       "~/.emacs.d/site-lisp"
           "~/.emacs.d/slime"
           "~/.emacs.d/yatex"
           "~/.emacs.d/ess/lisp/")
	      load-path))
;; yatex{{{3
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;; install-elisp{{{3
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/site-lisp")
;; for gosh{{{3
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8)
	    process-coding-system-alist))
;;; path gosh
(setq gosh-program-name "/usr/local/bin/gosh -i")
;;; use cmuscheme
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process" t)
;;; gosh-mode window
(defun my-scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
    (get-buffer-create "*scheme*"))
  (run-scheme gosh-program-name))
(add-hook 'scheme-mode-hook 'my-scheme-other-window)


;; ess{{{3
(require 'ess-site)
(defun my-ess-format-window ()
  "Open window for ESS. -- left window:code,upper right:help,lower right:ess"
  (split-window-horizontally)
  (other-window 1)
  (split-window)
  (other-window 1))
(add-hook 'ess-pre-run-hook 'my-ess-format-window)
(setq auto-mode-alist
      (cons (cons "\\.r$" 'R-mode) auto-mode-alist))
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
(setq ess-ask-for-ess-direstory nil)
;; tooltip-show-at-point, ess-R-object-tooltip.el {{{4
;;     http://sheephead.homelinux.org/2009/11/17/1699/
(add-hook 'ess-mode-hook
          (lambda()
           (require 'ess-R-object-tooltip)
           (define-key ess-mode-map "(" 'ess-r-args-auto-show)))
(setq ess-r-args-show-as 'tooltip)
;; slime{{{3
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup)
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))

;; viper{{{3
(setq viper-mode t)
(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '3)
(setq viper-ex-style-editing nil)
(require 'viper)
(require 'vimpulse)

;; http://ho-ki-boshi.blogspot.com/2007_11_01_archive.html
(setq viper-case-fold-search t)
(viper-buffer-search-enable)
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
(define-key viper-vi-global-user-map "\C-w\C-o" 'delete-other-windows)
(define-key viper-vi-global-user-map (kbd "/") 'viper-isearch-forward)
(define-key viper-vi-global-user-map (kbd "?") 'viper-isearch-backward)
;; dired を少しだけ vi 風に
(define-key viper-dired-modifier-map "j" 'dired-next-line)
(define-key viper-dired-modifier-map "k" 'dired-previous-line)
(define-key viper-dired-modifier-map "/" 'dired-goto-file)
(define-key viper-dired-modifier-map "l" '(lambda () (interactive) (dired-next-line 10)))
(define-key viper-dired-modifier-map "h" '(lambda () (interactive) (dired-previous-line 10)))
(define-key viper-insert-basic-map "\C-[" 'viper-intercept-ESC-key)
(viper-set-parsing-style-toggling-macro t)
;; :b := iswitchb :sp, :vs :q$
(setq ex-token-alist
    (append '(("buffer" (iswitchb-buffer))
  ("Buffer" (iswitchb-buffer-other-window))
  ("split" (split-window-vertically))
  ("vsplit" (split-window-horizontally))
  ("quit" (if (one-window-p)
       (ex-quit)
     (delete-window))))
       ex-token-alist))

;; *scratch* バッファで kill-buffer したら無視
(add-hook 'kill-buffer-query-functions
       (lambda () (not (string= "*scratch*" (buffer-name)))))

;; dabbrev-expand-multiple{{{3
;(require 'dabbrev-expand-multiple)
(global-set-key "\M-/" 'dabbrev-expand-multiple)

;; w3m-emacs{{3
(require 'w3m-load)
(setq w3m-default-display-inline-images t)

;; jaspace{{3
(require 'jaspace)

;; eshell{{{3
(setq eshell-glob-include-dot-dot nil)

;; uim.el{{{3
;(require 'uim)
;(autoload 'uim-mode "uim" nil t)
(global-set-key "\C-j" 'uim-mode)

;; uniquify -- buffer name modify{{{3
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;; c/migemo -- incremental searches by ro-maji{{{3
;;; base
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs" "-i" "\a"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict") ; PATH of migemo-dict
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
;;; use cache
(setq migemo-use-pattern-alist t)
(setq migemo-use-frequent-pattern-alist t)
(setq migemo-pattern-alist-length 1024)
;;; charset encoding
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
;;; initialization
(migemo-init)

;; linum -- line number{{{3
(require 'linum)
(global-linum-mode t)
(setq linum-format "%4d")
;; misc{{{3$
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(show-paren-mode 1)

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
(setq-default transient-mark-mode t)
(setq inhibit-startup-message t)

;マッピング
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\C-m" 'reindent-then-newline-and-indent)

(setq make-backup-files t)       ; バックアップファイルを作成する。
;;; バックアップファイルの保存場所を指定。
(setq backup-directory-alist
            (cons (cons "\\.*$" (expand-file-name "~/.backup"))
                              backup-directory-alist))

(setq version-control t)     ; 複数のバックアップを残します。世代。
(setq kept-new-versions 5)   ; 新しいものをいくつ残すか
(setq kept-old-versions 5)   ; 古いものをいくつ残すか
(setq delete-old-versions t) ; 確認せずに古いものを消す。
(setq vc-make-backup-files t) ; バージョン管理下のファイルもバックアップを作る。
;shebangが付いているファイルのパーミッションを保存時に +x にしてくれる設定
 (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;find-fileのファイル名補完で大文字小文字を区別しない設定

;(server-start)

(partial-completion-mode t)  ;ファイル名の一部で補完
(setq dired-recursive-copies 'always) ;再帰コピー ok
(setq dired-recursive-deletes 'always) ;再帰削除 ok
(require 'dired-x)   ;dired を拡張
(setq dired-listing-switches "-l") ;'.' ファイルを最初は表示しない
(ffap-bindings)    ;find file at point C-x C-f

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; moccur
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq moccur-split-word t)
;;;(setq moccur-use-migemo t)
;;(setq color-moccur-default-ime-status nil)
;;(autoload 'moccur-grep "color-moccur" nil t)
;;(autoload 'moccur-grep-find "color-moccur" nil t)
;;(autoload 'isearch-moccur "color-moccur" nil t)
;;(autoload 'isearch-moccur-all "color-moccur" nil t)
;;(autoload 'occur-by-moccur "color-moccur" nil t)
;;(autoload 'moccur "color-moccur" nil t)
;;(autoload 'dmoccur "color-moccur" nil t)
;;(autoload 'dired-do-moccur "color-moccur" nil t)
;;(autoload 'Buffer-menu-moccur "color-moccur" nil t)
;;(autoload 'moccur-narrow-down "color-moccur" nil t)
;;(autoload 'grep-buffers "color-moccur" nil t)
;;(autoload 'search-buffers "color-moccur" nil t)
;;(autoload 'gresreg "gresreg" nil t)
;;(eval-after-load "color-moccur"
;;  '(require 'moccur-edit))
;;(eval-after-load "ibuffer"
;;  '(require 'color-moccur))
;;(setq *moccur-buffer-name-exclusion-list*
;;      '(".*TAGS.*" "*Completions*" "*Messages*"
;; "newsrc.eld"
;; " *migemo*" ".bbdb"))
;;
;;(add-hook 'dired-mode-hook
;;   '(lambda ()
;;      (local-set-key "O" 'dired-do-moccur)))
;;(define-key Buffer-menu-mode-map "O" 'Buffer-menu-moccur)
;;;;(global-set-key "\M-f" 'grep-buffers)
;;(global-set-key "\M-o" 'occur-by-moccur)
;;(global-set-key "\C-c\C-x\C-o" 'moccur)
;;;;(global-set-key "\C-c\C-o" 'search-buffers)
;;
;;(setq dmoccur-recursive-search t)
;;
;;
;;;; カーソル付近の単語をデフォルトの検索文字列とする
;;(setq moccur-grep-default-word-near-point t)
;;
;;;; *.c 編集中のデフォルトファイルマスク： \.[HhCc]$
;;(add-hook
;; 'c-mode-common-hook
;; '(lambda ()
;;    (setq moccur-grep-default-mask "\\.\[HhCc\]$")))
;;(add-hook
;; 'perl-mode-hook
;; '(lambda ()
;;    (setq moccur-grep-default-mask "\\.pl$")))
;;(setq moccur-grep-following-mode-toggle t)
;;|#
;;
;;
;;;;;;;;;;;;;;;;;;
;;;; ac-mode
;;;;;;;;;;;;;;;;;;
;;(autoload 'ac-mode "ac-mode" "Minor mode for advanced completion." t nil)
;;
;;

;;; http://mi.cs.titech.ac.jp/kurihara/dotemacs.html$
;;; Version: $Id: my_lisp_init.el,v 1.89 2007/11/27 05:36:51 kurihara Exp $
;;; Last Modified: $Date: 2007/11/27 05:36:51 $
;;;

;; (setq stack-trace-on-error t)
;; (defun kerr (str)
;;   (save-excursion
;;     (set-buffer "*scratch*")
;;     (insert-string str)))

;;
;;;; apel の前に、default の load-path がないと、
;;;; apel 内部のプログラムが優先されてしまうことがある。
;;;; time-stamp.el は、apel の中にもあり、apel のものは古いので、
;;;; 問題がおきる。
;;(let (apels sorted-path)
;;  (while load-path
;;    (let ((head (car load-path)))
;;      (if (string-match "/apel" head)
;;          (setq apels (cons head apels))
;;        (setq sorted-path (cons head sorted-path)))
;;      (setq load-path (cdr load-path))))
;;  (setq load-path (append sorted-path apels)))
;;
;;(setq load-path
;;      ;; load-path に、マシンローカルの e-lisp があるので、
;;      ;; ~/ucvs 以下を優先させるべき。
;;      (append (list
;;               "~/lisp"
;;               "~/lisp/emacs-wget"
;;;;                "~/lisp/mule-ucs"
;;;;                "~/lisp/mule-ucs/jisx0213"
;;               "~/ucvs/share/emacs/site-lisp/apel"
;;               "~/ucvs/share/emacs/site-lisp/emu"
;;               "~/ucvs/share/emacs/site-lisp/flim"
;;               "~/ucvs/share/emacs/site-lisp/semi"
;;               "~/ucvs/share/emacs/site-lisp/wl"
;;               "~/ucvs/share/emacs/site-lisp/mew"
;;               "~/ucvs/share/emacs/site-lisp/w3m"
;;               "~/ucvs/share/emacs/site-lisp/skk"
;;               "~/ucvs/share/emacs/site-lisp/sdic"
;;;;                "~/ucvs/share/emacs/site-lisp/navi2ch"
;;               "~/ucvs/share/emacs/site-lisp/ruby"
;;               ;;"~/ucvs/share/emacs/site-lisp/yatex"
;;               ;;"~/ucvs/share/emacs/site-lisp/psgml"
;;;;                "~/ucvs/share/emacs/site-lisp/lookup"
;;               "~/ucvs/share/emacs/site-lisp"
;;               )
;;              load-path
;;              (list
;;               "/usr/local/share/emacs/site-lisp"
;;               )
;;              ))
;;
;;;; 初期設定
;;(when (fboundp 'utf-translate-cjk-mode)
;;;;   (provide 'un-define) ;; require 'un-define を、無意味にする
;;;;                        ;; un-define に定義されている関数が必要なこともある。
;;  (utf-translate-cjk-mode t)
;;;;  (require 'utf-8)
;;;;  (utf-translace-cjk-load-tables)
;;  )
;;
(setq
 ;; confirm when leaving emacs
 confirm-kill-emacs 'y-or-n-p
 ;; beep を止めて、flash screen にする。
 ;;visible-bell t
 ;; max-lisp-eval-depth の default は 300。wl で巨大なフォルダを s all
 ;; すると 300 を越えてしまう。
 max-lisp-eval-depth 1000
 ;; default は 1000
 max-specpdl-size 3000
 temporary-file-directory "~/tmp/"
 ;; Find-File 等で、ignore-case な補完
 completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;; ispell, flyspell のため。これを設定しないと、
 ;; 環境変数 LANG から言語が決まる。
 ispell-dictionary "english"
 ;; 日本語を含む文章の ps-print
 ps-multibyte-buffer 'non-latin-printer
 ;; signature-file
 ;; signature-file-name "~/vctrl/signature"
 ;; version-controll されているファイルのシンボリックリンクを辿るかどうか
 vc-follow-symlinks t
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
 ;; ignored-extensions like fignore
 completion-ignored-extensions (cons ".rtf" ;; rtf は emacs で開かない
                                     ;; pdf は、よく添付することがある。
                                     (remove ".dvi"
                                             (remove ".pdf"
                                                     completion-ignored-extensions)))
 ;; browse-url
 browse-url-browser-function (cond ((eq system-type 'darwin)
                                    'browse-url-default-macosx-browser)
                                   ;;(window-system 'browse-url-mozilla)
                                   (window-system 'w3m-browse-url)
                                   (t 'w3m-browse-url))
 ;; compile の時に、自動的にスクロール
 compilation-scroll-output t
 ;; コンパイルウィンドウの高さを指定
 compilation-window-height 8
 ;; non-nil で、undo が redo しない
 undo-no-redo t
 )

;;
;;;; yes or not を y or n にする
(fset 'yes-or-no-p 'y-or-n-p)
;;
;;;; 画像ファイルを表示
;;;(auto-image-file-mode t)
;
;;;; ファイルを開いた時に以前いた行に移動する
(setq-default save-place t)
(require 'saveplace)
;;
;;

;;;; dired-mode
;;(defun dired-up-directory-after-kill ()
;;  "Call 'dired-up-directory' after calling '(kill-buffer (current-buffer))'."
;;  (interactive)
;;  (let ((buf (current-buffer)))
;;    (dired-up-directory)
;;    (kill-buffer buf)))
;;(defun dired-advertised-find-file-after-kill ()
;;  "Call 'dired-advertised-find-file' after calling '(kill-buffer (current-buffer))'."
;;  (interactive)
;;  (let ((buf (current-buffer)))
;;    (dired-advertised-find-file)
;;    (kill-buffer buf)))
;;(add-hook 'dired-load-hook
;;          (require 'dired-x)
;;          )
;;(defun my-dired-mode-hook-fun ()
;;  (setq
;;   ;; C-x C-j に dired-jump を割当てる。 dired-jump は、current buffer を dired で開く。
;;   dired-bind-jump nil
;;   dired-bind-man nil
;;   dired-bind-info nil
;;   dired-guess-shell-alist-user
;;   (remq nil$
;;         (list
;;          (if (eq window-system 'mac)$
;;              (list "\\.\\(pdf\\|jpg\\|tiff\\|html\\|htm\\|eps\\)$" "open"))
;;          (list "\\.tex$" "latex" "platex")
;;          (list "\\.dvi$" "xdvi -display :0.0 ? &" "dvips" "dvipdfmx")
;;          (list "\\.ps$" "open" "ps2pdf" "lpr")
;;          ))
;;   dired-recursive-copies 'ask
;;   dired-recursive-deletes 'ask
;;   dired-listing-switches "-alh"
;;   )
;;  (set-face-foreground 'dired-marked "GreenYellow")
;;  (define-key dired-mode-map "\M-o" nil) ;; load-hook では、駄目なことがある
;;  (define-key dired-mode-map "\M-^" 'dired-up-directory-after-kill)
;;  (define-key dired-mode-map "\M-\C-m" 'dired-advertised-find-file-after-kill)
;;  (define-key dired-mode-map "\C-co" 'dired-omit-mode)
;;  (if (my-locate-file ".tex" (pwd))
;;      (add-to-list 'dired-omit-extensions ".log")
;;    (setq dired-omit-extensions (remove ".log" dired-omit-extensions)))
;;  (dired-omit-mode 1) ;; dot file や、.o ファイル が省略される
;;  )
;;(defun my-locate-file (suffix dir)
;;  ;; return non-nil or nil
;;  (with-temp-buffer
;;    (goto-char (point-min))
;;    (call-process "ls" nil t t (expand-file-name dir))
;;    (goto-char (point-min))
;;    (search-forward-regexp (concat "^.*" suffix "$") (point-max) t)))
;;
;;   $
;;
;;(add-hook 'dired-mode-hook
;;          'my-dired-mode-hook-fun)
;;
;;
;;;; font-lock mode カラーモード
;;(require 'font-lock)
;;(global-font-lock-mode 1)
;;(add-hook 'font-lock-mode-hook$
;;          '(lambda ()
;;             (set-face-foreground 'font-lock-builtin-face "violet")
;;             (set-face-foreground 'font-lock-comment-face "#ff2222")
;;             (set-face-foreground 'font-lock-string-face  "sandy brown")
;;             (set-face-foreground 'font-lock-keyword-face "skyblue")
;;             (set-face-foreground 'font-lock-constant-face "dark orange")
;;             (set-face-foreground 'font-lock-function-name-face "skyblue")
;;             (set-face-foreground 'font-lock-variable-name-face "dark orange")
;;             (set-face-foreground 'font-lock-type-face "LightSeaGreen")
;;             (set-face-foreground 'font-lock-warning-face "skyblue")
;;             (set-face-bold-p 'font-lock-function-name-face t)
;;             (set-face-bold-p 'font-lock-warning-face nil)
;;             ))
;;
;; show-paren-mode
(add-hook 'show-paren-mode-hook
          (lambda ()
            (set-face-background 'show-paren-match-face "DarkGoldenrod4")))
(add-hook 'find-file-hooks
          (lambda ()
            (show-paren-mode t))); 起動の時間を軽減

;;;; abbrev
;;(setq abbrev-file-name "~/ucvs/etc/abbrev_defs"
;;      save-abbrevs t)
;;(quietly-read-abbrev-file)
;;
;;;; カーソルの点滅をやめる
(blink-cursor-mode 0)
;;
;;;; メニューバーを表示しない ; Xdefaultsでも設定してある。
;;
;;
;; 行をハイライト
(if window-system
    (progn
      (require 'highline)
      (highline-mode)
      (setq highline-face '((t (:background "#222222"))))
      ))

;; インデント関係
;; (setq standard-indent 4) ; default は 4
(add-hook 'find-file-hooks$
          (lambda ()$
            ;; indent-tabs-mode は buffer local
            (if (or (memq major-mode '(change-log-mode))
                    (string-match "^makefile.*mode$" (prin1-to-string major-mode)))
                (setq indent-tabs-mode t)
              (setq indent-tabs-mode nil)) ; tab の代わりに space を使用
            ;;(setq tab-width 4)
            ))

;;;; 選択範囲について
;;;;$
;;;; macのような範囲選択のキーバインドを実現する
;;;; transient と、delete-selection も設定する
;;;; 選択範囲に色を付ける
;;(set-face-background 'region "RoyalBlue4")
;;;; ハイライトされていなくても、選択範囲が存在する
(setq mark-even-if-inactive t)
;; ペーストする時に選択範囲があれば、それを上書き
(delete-selection-mode 1)
;;
(setq woman-use-own-frame nil)
(setq woman-cache-filename "~/.woman.cache")
;;
;;;;;
;;;;; diff-mode
;;;;;
(eval-when-compile
  (load "diff-mode"))
(add-hook 'diff-mode-hook
          (lambda ()
            (define-key diff-mode-map "\M-o" nil)))

;;;
;;; byte-compile
;;;
(eval-after-load "bytecomp"
  '(lambda ()
     ;; byte-compile で、warning する項目
     ;; default -> t -> all
     ;; key: redefine callargs free-vars unresolved obsolete noruntime
     ;;      cl-functions interactive-only
     (setq byte-compile-warnings '(redefine callargs free-vars unresolved$
                                            obsolete cl-functions))))

;;;;;
;;;;; make-file-mode
;;;;;
;;(add-hook 'makefile-mode-hook
;;          '(lambda ()
;;             (define-key makefile-mode-map "\C-c\C-u" 'uncomment-region)))
;;
;;;;; -------------------------------ここから-------------------------------
;;;;; 外部 elisp ライブラリ
;;;;;
;;
;;
;;;;; sesion.el
;;(require 'session)
;;(add-hook 'after-init-hook 'session-initialize)
;;
;;;;; redo
(require 'redo)
;;
;;
;;;;;
;;;:; 自作の小物
;;;;;
;;(autoload 'open "my_lisp_lazy-loads" "open a file with OSX native app" t)
;;
;;
;;;;; 物理行移動
;;(autoload 'physical-line-mode "physical-line" "" t)
;;;(add-hook 'find-file-hooks 'physical-line-mode )
;;;;;
;;;;; lookup.el
;;;;;
;;;; (autoload 'lookup "lookup" nil t)
;;;; (autoload 'lookup-region "lookup" nil t)
;;;; (autoload 'lookup-pattern "lookup" nil t)
;;;; (setq lookup-search-agents
;;;; ;;       '((ndtp "localhost" :appendix "/usr/local/share/dict/appendix/chujiten")
;;;;       '((ndtp "localhost")
;;;;         (ndspell)
;;;;         )
;;;;       lookup-use-bitmap nil; # default t
;;;;       )
;;
;;;; (autoload 'lookup-entry-search-pattern "lookup-entry" nil t)
;;;; 最初に M-x lookup する必要がある。理由はおいかけてないので、不明。
;;;; おそらく、辞書の選択等の設定が必要ということ。
;;;; (global-set-key "\C-ql" 'lookup-entry-search-pattern)
;;
;;
;;;;;
;;;;; windows.elカスタム
;;;;;
;;(eval-when-compile
;;  (require 'windows))
;;;; windowsのプレフィックスを変更; 順序が重要
;;(defvar win:switch-prefix "\C-q\C-w")
;;(require 'windows)
;;(define-key global-map win:switch-prefix win:switch-map)
;;(define-key global-map "\C-xc" 'see-you-again)
;;(win:startup-with-window)
;;;; ウィンドウの切替に frame を使わない
;;(setq win:use-frame nil)
;;;; 新規ウィンドウの位置を(0, 0)に
;;;(setq win:new-frame-offset-x 0)
;;;(setq win:new-frame-offset-y 0)
;;;; (defun my-init-windows ()
;;;;   (require 'windows)
;;;;   (define-key global-map win:switch-prefix win:switch-map)
;;;;   (define-key global-map "\C-xc" 'see-you-again)
;;;;   (setq win:use-frame nil)
;;;;   (win:startup-with-window)
;;;;   ;; ウィンドウの切替に frame を使わない
;;;;   (win:switch-window 1 t t);; 一旦、[1]に移動
;;;;   )
;;
;;;; C-q C-s で、ウィンドウを保存 == C-q C-w C-r s
;;(global-set-key "\C-q\C-s"
;;                (lambda ()
;;                  (interactive)
;;;;                   (my-init-windows)
;;                  (win:save-window win:current-config)))
;;;; C-q C-r で、ウィンドウをリジューム
;;(global-set-key "\C-q\C-r"
;;                (lambda ()
;;                  (interactive)
;;;;                   (my-init-windows)
;;                  (win-resume-menu)))
;;
;;;; (defadvice win:update-mode-line (after make-color activate)
;;;;   (put-text-property 1 (- (length win:mode-string) 1)
;;;;                   'face '(:foreground "red3" :weight bold)
;;;;                   win:mode-string))
;;
;;
;;
;;;;; emacs-wget
;;(eval-when-compile
;;  ;;(require 'w3m)
;;  (require 'wget))
;;(autoload 'wget "wget" "wget interface for Emacs." t)
;;(autoload 'wget-web-page "wget" "wget interface to download whole web page." t)
;;(add-hook 'wget-load-hook
;;          (lambda ()
;;            (load "w3m-wget")
;;            (setq wget-basic-options '("-v"))))
;;(add-hook 'w3m-mode-hook
;;          (lambda ()
;;            (load "w3m-wget")))
;;
;;;;; tramp
;;;;;
;;;;; syntax: /sato:.emacs
;;;;;
;;;; tramp は、find-file の時に、syntax が tarmp ならば、auto-load される。
;;;(require 'tramp)
;;(setq tramp-default-method "sshx"
;;      tramp-verbose 5)
;;;; (defun tramp ()
;;;;   (interactive)
;;;;   (require 'tramp))
;;
;;;;;
;;;;; minibuf-isearch
;;;;;
;;;; mini-buffer内で tcsh のように、C-rで履歴のインクリメンタルサーチ
;;;; (add-hook 'minibuffer-setup-hook
;;;;           (lambda ()
;;;;             (require 'minibuf-isearch))) ; 結局、起動時に読まれる
;;(require 'minibuf-isearch)
;;(setq minibuf-isearch-use-migemo nil
;;      minibuf-isearch-treat-filecache nil)
;;
;;;; (add-hook 'minibuf-isearch-mode-hook
;;;;           (lambda () (require 'filecache)))
;;
;;;; ;;;
;;;; ;;; filecache
;;;; ;;;
;;;; (setq file-cache t)
;;;; (file-cache-add-directory-recursively "~/Documents") ;; this takes long time
;;;; (file-cache-add-directory-recursively "~/work") ;; this takes long time as well
;;
;;
;;;;;
;;;;; viper ; session.el の後に読まないと session が効かなくなるようだ
;;;;;
;;(setq viper-custom-file-name "~/lisp/my_lisp_viper-custom.el"
;;      viper-ESC-key "\e"
;;      viper-mode t)
;;(require 'viper)
;;(when (facep 'viper-minibuffer-emacs)
;;  (set-face-foreground 'viper-minibuffer-emacs "white")
;;  (set-face-background 'viper-minibuffer-emacs "black"))
;;(setq viper-emacs-state-mode-list (append viper-emacs-state-mode-list
;;                                          '(occur-mode
;;                                            todo-mode
;;                                            shell-mode
;;                                            term-mode
;;                                            wl-folder-mode
;;                                            wl-summary-mode
;;                                            wl-template-mode
;;                                            ))
;;      viper-vi-state-mode-list (append viper-vi-state-mode-list
;;                                       '(wl-draft-mode
;;                                         apropos-mode
;;                                         )))
;;(define-key viper-insert-basic-map "\C-d" 'delete-char)
;;(add-hook 'viper-vi-state-hook
;;          '(lambda ()
;;             ;; vi-state でカーソルの色を変える。
;;             ;; この変数は内部変数なので、ただ setq しても駄目
;;             (setq viper-vi-state-cursor-color "dark orange")
;;             ))
;;(add-hook 'viper-emacs-state-hook
;;          '(lambda ()
;;             ;; vi-state でカーソルの色を変える。
;;             ;; この変数は内部変数なので、ただ setq しても駄目
;;             (set-cursor-color "yellow")
;;             ))
;;
;;;; viper を読んだ後に、minor-mode の keymap を define するものは、
;;;; harness を設定しないと、define がうまくいかない。
;;(viper-harness-minor-mode "mime-edit")
;;(viper-harness-minor-mode "abbrev")
;;
;;
;;;;;
;;;;; iswitchb
;;;;;
;;(iswitchb-mode 1)
;;(iswitchb-default-keybindings)
;;(setq iswitchb-default-method 'samewindow)
;;
;;
;;;;; Wanderlust
;;;;;
;;;; autoload
;;(autoload 'wl "wl" "Wanderlust" t)
;;(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
;;;; wanderlust を emacs の標準 mail-user-agent として設定する
;;(autoload 'wl-user-agent-compose "wl-draft" nil t)
;;(define-mail-user-agent
;;  'wl-user-agent
;;  'wl-user-agent-compose
;;  'wl-draft-send
;;  'wl-draft-kill
;;  'mail-send-hook)
;;(setq mail-user-agent 'wl-user-agent
;;      ;; icon, addresses は init-hook 以前に設定しなければいけない
;;      wl-icon-directory "~/ucvs/share/emacs/21.3/etc/wl"
;;      wl-address-file "~/lisp/addresses"
;;      ;; folders に、shimbun が含まれていると、w3m もロードされる。
;;      wl-folders-file "~/lisp/folders"
;;      wl-init-file "my_lisp_wl")
;;
;;;;; skk.el
;;;;;
;;(eval-when-compile
;;  (require 'skk-autoloads)
;;  (require 'skk)
;;  (require 'my_lisp_skk))
;;; 基本的な設定を自動でしてくれる
;;; (require 'skk-setup)
;;
;;;; wanderlust の起動後に skk を起動すると、free variable だと
;;;; 怒られるのを回避
;;(defvar skk-isearch-switch nil)
;;
;;;; 基本の設定
;;(global-set-key "\C-x\C-j"$
;;                (lambda ()
;;                  (interactive)
;;                  (require 'skk-autoloads)
;;                  (skk-mode)))
;;(setq skk-init-file "my_lisp_skk")
;;
;;;;;
;;;;; word-count-mode
;;;;;
;;(autoload 'word-count-mode "word-count"
;;          "Minor mode to count words." t nil)
;;(global-set-key "\M-+" 'word-count-mode)
;;
;;;;;
;;
;;;;; migemo
;;;;;
;;(when (locate-library "migemo")
;;  (setq migemo-directory "~/ucvs/share/migemo")
;;  (load "migemo")
;;;;   (migemo-init)
;;  ;;C-q m で migemo-toggle
;;  (autoload 'my-interactive-migemo-toggle "my_lisp_lazy-loads" "toggle migemo" t)
;;  (global-set-key "\C-qm" 'my-interactive-migemo-toggle)
;;  (setq
;;   ;; 正規表現のリストをキャッシュとして保存
;;   migemo-use-pattern-alist nil
;;   migemo-use-frequent-pattern-alist nil
;;   ;; リストの長さ。デフォルトは 512
;;   migemo-pattern-alist-length 1024
;;   ;; isearch を開始する長さ
;;   migemo-isearch-min-length 2)
;;  )
;;
