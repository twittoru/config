;-*-Emacs-Lisp-*-
(defconst vip-slime-leader " ")
(defun viper-slime-key (key func)
  (define-key viper-vi-global-user-map (concat vip-slime-leader key) func))

; In general the Viper Slime mappings are much the same as regular Slime bindings,
; The C-c and C-x prefixes are dropped.
; When commands are similar, we use a lower case letter for the C-<key> case and an upper
; case letter for the M-<key>, such as:
; C-c C-k : slime-compile-and-load-file  : (vip-slime-leader k)
; C-c M-k : slime-compile-file           : (vip-slime-leader K)
; All commands begin with vip-slime-leader, which defaults to <space>
; the M-x commands are not mapped, as they are presumably rare
; Some keys are a triple key sequence.  The second key is a marker for a category
; the third key is the activation key.
(add-hook 'slime-mode-hook 'do-viper-slime-mappings) 
(defun do-viper-slime-mappings ()
  (define-key viper-vi-global-user-map vip-slime-leader (make-sparse-keymap))
; Compilation commands
  (viper-slime-key "k" 'slime-compile-and-load-file)
  (viper-slime-key "K" 'slime-compile-file)
  (viper-slime-key "c" 'slime-compile-defun)
  (viper-slime-key "C" 'slime-remove-notes)
; Note handling has the same binding as Slime defaults
  (define-key viper-vi-global-user-map "M-n" 'slime-next-note)
  (define-key viper-vi-global-user-map "M-p" 'slime-previous-note)

; Finding definitions (they are same as Slime default)
  (define-key viper-vi-global-user-map "M-." 'slime-edit-definition)   
  (define-key viper-vi-global-user-map "M-," 'slime-pop-find-definition-stack)

; Lisp Evaluation
  (viper-slime-key "x" 'slime-eval-defun)
  (viper-slime-key "e" 'slime-eval-last-expression)
  (viper-slime-key "p" 'slime-pprint-eval-last-expression)
  (viper-slime-key "r" 'slime-eval-region)

; Lisp Documentation
; All documentation functions are triple key sequences (vip-slime-leader ?d key)
  (viper-slime-key "d" (make-sparse-keymap))
  (viper-slime-key "dd" 'slime-describe-symbol) 
  (viper-slime-key "da" 'slime-apropos) 
  (viper-slime-key "dz" 'slime-apropos-all)
  (viper-slime-key "dp" 'slime-apropos-package)
  (viper-slime-key "dh" 'slime-hyperspec-lookup)
  (viper-slime-key "d~" 'common-lisp-hyperspec-format)

; Programming helpers
; Completion TODO
; Completion is not yet done, as the bindings will need to be in Insert mode to be really useful

; Macro expansion
  (viper-slime-key "m" 'slime-macroexpand-1)
  (viper-slime-key "M" 'slime-macroexpand-all)
  (viper-slime-key "t" 'slime-toggle-trace-fdefinition)

; Disassembly
  (viper-slime-key "D" 'slime-disassemble-symbol)

; Abort/Recovery
  (viper-slime-key "b" 'slime-interrupt)
  (viper-slime-key "~" 'slime-sync-package-and-default-directory)
  (viper-slime-key "P" 'slime-repl-set-package)

; Cross-reference
; All cross-reference functions are triple key sequences (vip-slime-leader ?w key)
  (viper-slime-key "w" (make-sparse-keymap))
  (viper-slime-key "wc" 'slime-who-calls)
  (viper-slime-key "wr" 'slime-who-references)
  (viper-slime-key "wb" 'slime-who-binds)
  (viper-slime-key "ws" 'slime-who-sets)
  (viper-slime-key "wm" 'slime-who-macroexpands)
  (viper-slime-key "<" 'slime-list-callers)
  (viper-slime-key "<" 'slime-list-callees)

; Inspector
  (viper-slime-key "i" 'slime-inspect)

; Profiler
; "p" is already taken as a key, we use "f" to access the profiler functions
  (viper-slime-key "f" (make-sparse-keymap))
  (viper-slime-key "ft" 'slime-toggle-profile-fdefinition)
  (viper-slime-key "fp" 'slime-profile-package)
  (viper-slime-key "fu" 'slime-unprofile-all)
  (viper-slime-key "fr" 'slime-profile-report)
  (viper-slime-key "fR" 'slime-profile-reset))

(add-hook 'slime-inspector-mode-hook 'viper-inspector-mappings)
(defun viper-inspector-mappings ()
  ; The default for POP is "l", but we use that up
  (define-key slime-inspector-mode-map "p" 'slime-inspector-pop)
  (viper-modify-map-for-movement slime-inspector-mode-map)
  (run-hooks 'viper-slime-inspector-mapping-hook))
  
(add-hook 'sldb-mode-hook 'viper-sldb-mappings)
(defun viper-sldb-mappings ()
  (viper-modify-map-for-movement sldb-mode-map)
  (run-hooks 'viper-sldb-mapping-hook))

(add-hook 'slime-xref-mode-hook 'viper-xref-mappings)
(defun viper-xref-mappings ()
  (viper-modify-map-for-movement slime-xref-mode-map)
  (run-hooks 'viper-xref-mapping-hook))
