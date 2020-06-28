;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-src-tab-acts-natively t)
(setq org-directory "~/Dropbox/org/")
(setq org-capture-templates
      '(("p" "Project Task" entry (file+headline "~/Dropbox/org/project/project.org" "Inbox")
             "** TODO %?\n")
        ("n" "notes" entry (file "~/Dropbox/org/notes.org")
         "* %?%t")
        ("m" "Meetings memo" entry (file+olp+datetree "~/Dropbox/org/project/mtg.org")
         "* %?%t")))
(setq org-agenda-files (list "~/Dropbox/org/project"))
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(nyan-mode t)

;; sublimity
;; (require 'sublimity-scroll)
;; (sublimity-mode t)

;;; lsp-ui
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-symbol t)
(setq lsp-ui-peek-enable t)
(setq lsp-ui-peek-peek-height 20)
(setq lsp-ui-peek-list-width 50)
(setq lsp-ui-peek-fontify 'on-demand) ;; never, on-demand, or always
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-doc-position 'top) ;; top, bottom, or at-point
(setq lsp-ui-doc-max-width 150)
(setq lsp-ui-doc-max-height 30)
(setq lsp-ui-doc-use-childframe t)
(setq lsp-ui-doc-use-webkit t)
(setq lsp-ui-imenu-enable t)

;; ;キーで自動的にセミコロンを挿入
(defun bb-semicolon ()
  (interactive)
  (save-excursion
  (end-of-line)
  (insert ";")))
(with-eval-after-load 'evil-maps
(define-key evil-normal-state-map (kbd ";") 'bb-semicolon))

;; tsserver（tide）のログ出力先設定
(setq tide-tsserver-process-environment '("TSS_LOG=level verbose -file /tmp/tss.log"))

;; インデントの設定
(add-hook 'js2-mode-hook
  (lambda ()
    (setq my-js-mode-indent-num 2)
    (setq js2-basic-offset my-js-mode-indent-num)
    (setq js-switch-indent-offset my-js-mode-indent-num)
    ))
(add-hook 'json-mode-hook
  (lambda ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2)))
(add-hook 'scss-mode-hook
  (lambda ()
    (setq css-indent-offset 2)))
(add-hook 'typescript-mode-hook
  (lambda ()
    (make-local-variable 'typescript-indent-level)
    (setq typescript-indent-level 2)))
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  ())
(add-hook 'web-mode-hook 'web-mode-hook)
(setq tide-format-options '(:indentSize 2 :tabSize 2))
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'css-stylelint 'web-mode))
;;   (add-hook 'typescript-mode-hook (lambda () (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)))
;;   (add-hook 'typescript-mode-hook (lambda () (flycheck-add-next-checker 'javascript-eslint 'css-stylelint))))
(add-hook 'web-mode-hook 'web-mode-hook)

;; 軽量化
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))
(setq scroll-bar-mode nil)
(require 'hl-line)
;;; hl-lineを無効にするメジャーモードを指定する
(defvar global-hl-line-timer-exclude-modes '(todotxt-mode))
(defun global-hl-line-timer-function ()
  (unless (memq major-mode global-hl-line-timer-exclude-modes)
    (global-hl-line-unhighlight-all)
    (let ((global-hl-line-mode t))
      (global-hl-line-highlight))))
(setq global-hl-line-timer
      (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
;; (cancel-timer global-hl-line-timer)
;; (set-frame-parameter nil 'alpha 95)

;; projectileのルート設定
(setq projectile-project-root-files #'( ".projectile" ))
(setq projectile-project-root-files-functions #'(projectile-root-top-down
                                                 projectile-root-top-down-recurring
                                                 projectile-root-bottom-up
                                                 projectile-root-local))

;; ts,tsxでtideではなくts-lintを使用する
;; https://github.com/hlissner/doom-emacs/issues/2395#issuecomment-578920992
;; (define-derived-mode ts-mode web-mode "TypeScript Mode")
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
;; (after! flycheck
;;   (flycheck-add-mode 'javascript-eslint 'typescript-mode)
;;   (flycheck-add-mode 'css-stylelint 'typescript-mode)
;;   (add-hook 'typescript-mode-hook (lambda () (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)))
;;   (add-hook 'typescript-mode-hook (lambda () (flycheck-add-next-checker 'javascript-eslint 'css-stylelint))))
