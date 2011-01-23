;; -*- filetype lisp -*-
;; vim: set filetype=lisp :
;;{{{options

(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '3)
;;; set autoindent shiftwidth=4
(setq-default viper-auto-indent t) 
(setq viper-shift-width 4)
(setq ex-cycle-other-window nil) 
;;; f,F,t,T はcursorがある行にのみ働く。
(setq viper-allow-multiline-replace-regions nil)
(setq viper-case-fold-search t)
(viper-buffer-search-enable)
(viper-set-parsing-style-toggling-macro t)

;;}}}
;;{{{functions

;;;http://blog.livedoor.jp/tek_nishi/archives/2831992.html
(defun my-isearch-get-word()
  "カーソル位置の単語をisearch"
  (interactive)
  (if(not isearch-mode)
      (call-interactively 'isearch-forward)))

(defun my-isearch-get-word-hook()
  (when (equal this-command 'my-isearch-get-word)
    (let ((string
           (if (and transient-mark-mode mark-active)
               (buffer-substring (region-beginning) (region-end))
             (thing-at-point 'symbol))))
      (deactivate-mark)
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))
(add-hook 'isearch-mode-hook 'my-isearch-get-word-hook)

;;; http://ho-ki-boshi.blogspot.com/2007_11_01_archive.html
(defun my-viper-beginning-of-buffer ()
  (interactive)
  (beginning-of-buffer))
(defun my-viper-star ()
  (interactive)
  (let ((wd (concat "\\<" (thing-at-point 'symbol) "\\>")))
    (setq viper-s-string wd)
    (setq viper-s-forward t)
    (viper-search wd t 1)))
(defun my-viper-jump-tag ()
  (interactive)
  (setq wd (thing-at-point 'symbol))
  (find-tag wd))
(defun my-viper-jump-tag-next ()
  (interactive)
  (setq wd (thing-at-point 'symbol))
  (find-tag wd 0))
(defun my-viper-pop-tag ()
  (interactive)
  (pop-tag-mark))
(defun my-viper-pop-mark ()
  (interactive)
  (set-mark-command -1))
(defun previous-window-line (n)
  (interactive "p")
  (let ((cur-col
         (- (current-column)
            (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion (- n))
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook))
(defun next-window-line (n)
  (interactive "p")
  (let ((cur-col
         (- (current-column)
            (save-excursion (vertical-motion 0) (current-column)))))
    (vertical-motion n)
    (move-to-column (+ (current-column) cur-col)))
  (run-hooks 'auto-line-hook))

;;}}}
;;{{{keymaps
(define-key viper-vi-global-user-map [?j]  'next-window-line)
(define-key viper-vi-global-user-map [?k]  'previous-window-line)
(define-key viper-vi-global-user-map [?z?o]       'folding-show-current-entry)
(define-key viper-vi-global-user-map [?z?c]     'folding-hide-current-entry)
(define-key viper-vi-global-user-map [?;]       'viper-ex)
(define-key viper-vi-global-user-map [?g?f]     'find-file-at-point)
(define-key viper-vi-global-user-map [?z?t]     'viper-line-to-top)
(define-key viper-vi-global-user-map [?z?b]     'viper-line-to-bottom)
(define-key viper-vi-global-user-map [?z?z]     'viper-line-to-middle)
(define-key viper-vi-global-user-map [?g?g]     'my-viper-beginning-of-buffer)
;(define-key viper-vi-global-user-map [?*]       'my-viper-star)
(define-key viper-vi-global-user-map [?*]       'my-isearch-get-word-hook)
(define-key viper-vi-global-user-map [?\C-]]    'my-viper-jump-tag)
(define-key viper-vi-global-user-map [?\C-:]    'my-viper-jump-tag-next)
(define-key viper-vi-global-user-map [?\C-t]    'my-viper-pop-tag)
(define-key viper-vi-global-user-map [?\C-o]    'my-viper-pop-mark)
(define-key viper-vi-global-user-map [?u]       'undo-tree-visualize)
(define-key viper-vi-global-user-map [?\C-r]    'redo)
(define-key viper-vi-global-user-map [?\C-w?\C-o] 'delete-other-windows)
(define-key viper-vi-global-user-map [?/]       'viper-isearch-forward)
(define-key viper-vi-global-user-map [??]       'viper-isearch-backward)
(define-key viper-vi-global-user-map [?x]       'delete-char)
(define-key viper-insert-global-user-map [?\C-h] 'backward-delete-char-untabify)
(define-key viper-insert-global-user-map [?\C-d] 'delete-char)
(define-key viper-insert-global-user-map [backspace] 'backward-delete-char-untabify)
(define-key viper-insert-global-user-map [?\C-c] 'viper-exit-insert-state)
(define-key viper-insert-global-user-map [delete] 'delete-char)
(define-key viper-insert-global-user-map [?\C-n]  'dabbrev-expand)


;;; window operation from http://d.hatena.ne.jp/iicebar/20100512/1273681113
(define-key viper-emacs-global-user-map "\C-w\C-w" 'other-window)
(define-key viper-vi-global-user-map "\C-w\C-w" 'other-window)
(define-key viper-emacs-global-user-map "\C-w\C-o" 'delete-other-windows)
(define-key viper-vi-global-user-map "\C-w\C-o" 'delete-other-windows)
(define-key viper-vi-global-user-map "\C-w\h" 'windmove-left)
(define-key viper-vi-global-user-map "\C-w\j" 'windmove-down)
(define-key viper-vi-global-user-map "\C-w\k" 'windmove-up)
(define-key viper-vi-global-user-map "\C-w\l" 'windmove-right)
(define-key viper-emacs-global-user-map "\C-w\h" 'windmove-left)
(define-key viper-emacs-global-user-map "\C-w\j" 'windmove-down)
(define-key viper-emacs-global-user-map "\C-w\k" 'windmove-up)
(define-key viper-emacs-global-user-map "\C-w\l" 'windmove-right)

;;; dired を少しだけ vi 風に
(define-key viper-dired-modifier-map [?j]       'dired-next-line)
(define-key viper-dired-modifier-map [?k]       'dired-previous-line)
(define-key viper-dired-modifier-map [?/]       'dired-goto-file)
(define-key viper-dired-modifier-map [?l]
  '(lambda () (interactive) (dired-next-line 10)))
(define-key viper-dired-modifier-map [?h]
  '(lambda () (interactive) (dired-previous-line 10)))
(define-key viper-insert-global-user-map "\C-[" 'viper-intercept-ESC-key)

;;}}}
;;{{{ex-commands

;;; Additional Ex mode features.
(defvar viper-extra-ex-commands '(
                                  ("sp" "split")
                                  ("e" "edit")
                                  ("b" "buffer")
                                  ("bd" "bdelete")
                                  ("bn" "next")
                                  ("gr" "grep")
                                  ("vs" "vsplit")
                                  ("grep" (viper-grep))
                                  ("bdelete" (viper-kill-current-buffer))
                                  ("buffer" (iswitchb-buffer))
                                  ("Buffer" (iswitchb-buffer-other-window))
                                  ("split" (split-window-vertically))
                                  ("vsplit" (split-window-horizontally))
                                  ("quit" (if (one-window-p)
                                            (ex-quit)
                                            (delete-window)))))
(setq ex-token-alist (append viper-extra-ex-commands ex-token-alist))
;;; *scratch* バッファで kill-buffer したら無視
(add-hook 'kill-buffer-query-functions
          (lambda () (not (string= "*scratch*" (buffer-name)))))

;;}}}
