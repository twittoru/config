;; dot.emacs
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
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;;}}}
;;{{{imaxima
(autoload 'imaxima "imaxima" "Frontend of Maxima CAS" t)
(autoload 'imath "imath" "Interactive Math mode" t)
(autoload 'imath-mode "imath" "Interactive Math mode" t)
;;}}}
;;{{{auto-install
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/site-lisp/")
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
;;}}}
;;{{{gosh
(setq prooding-system-alist
      (cons '("gosh" utf-8 . utf-8)
            process-coding-system-alist)
      gosh-program-name "/usr/local/bin/gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme" t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process" t)
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
(add-hook 'lisp-mode-hook
          (lambda ()
            (cond ((not (featurep 'slime))
                   (require 'slime) 
                   (normal-mode)))))
;(require 'slime)
;;}}}
;;{{{ess
(setq auto-mode-alist
      (cons (cons "\\.R$" 'R-mode) auto-mode-alist))
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
(autoload 'R "ess-site" nil 'interactive)
(setq ess-ask-for-ess-directory nil)
(setq ess-pre-run-hook
      '((lambda ()
          (setq default-process-coding-system '(utf-8 . utf-8))
          ;; iess-mode keymapping for viper
          (define-key inferior-ess-mode-map [return] 'inferior-ess-send-input)
          )))
(require 'align)
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

(require 'redo)
(require 'viper)
(require 'vimpulse)

(define-key vimpulse-visual-global-user-map [?j]  'next-line)
(define-key vimpulse-visual-global-user-map [?k]  'previous-line)

(defadvice viper-change-cursor-color
           ;;チラツキ解消 -- 発現していないが導入しておく
           ;;http://d.hatena.ne.jp/leque/20100304/p2
           (around kludge-to-avoid-cursor-flicker-on-Cocoa-Emacs activate)
           nil)
;;}}}
;;{{{w3m-emacs

(require 'w3m-load)
(setq w3m-default-display-inline-images t)

;;}}}
;;{{{jaspace

(require 'jaspace)

;;}}}
;;{{{eshell

(setq eshell-glob-include-dot-dot nil)
;;; http://www.emacswiki.org/emacs/EshellAndViper
(add-hook 'eshell-mode-hook
          (lambda ()
            (when viper-mode
              (setq viper-auto-indent nil))))

;;}}}
;;{{{uniquify

;;;buffer name modify
(require 'uniquify)
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

(require 'linum)
(global-linum-mode t)
(setq linum-format "%4d")

;;}}}
;;{{{folding -- vimlike fold
(load "folding" 'nomessage 'noerror)
(folding-mode-add-find-file-hook)
(folding-add-to-marks-list 'emacs-lisp-mode ";;{{{" ";;}}}" nil t)
;;}}}
;;{{{skk

(require 'skk-autoloads)
(setq skk-kuten-touten-alist
      '(
        (jp . ("。" . "、"))
        (en . ("." . ","))
        ))
(setq-default skk-kutoten-type 'en)
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
(require 'undo-tree)
(global-undo-tree-mode)
; auto-set insert-mode to use vi-keybind in visualizer-mode
; undo-tree has not any hook ><
(defadvice undo-tree-visualizer-mode
           (after viper-undo-tree-visualizer-mode activate)
           (viper-insert t))

(define-key undo-tree-visualizer-map [?k] 'undo-tree-visualize-undo) 
(define-key undo-tree-visualizer-map [?j] 'undo-tree-visualize-redo) 
(define-key undo-tree-visualizer-map [?h] 'undo-tree-visualize-switch-branch-left)
(define-key undo-tree-visualizer-map [?l] 'undo-tree-visualize-switch-branch-right)
(define-key undo-tree-visualizer-map [?q] 'undo-tree-visualizer-quit) 

;;}}}
;;{{{navi2ch
(autoload 'navi2ch' "navi2ch" "Navigator for 2ch for Emacs" t)
;;}}}
;;{{{misc

(if (not window-system)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (display-time-mode 1)
  )

;font from http://sakito.jp/emacs/emacs23.html
(when (>= emacs-major-version 23)
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
          ("-cdac$" . 1.3))))

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
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

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
(require 'saveplace)
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

;;}}}
