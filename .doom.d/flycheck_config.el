;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Transparency
(add-to-list 'default-frame-alist '(alpha-background . 80))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(custom-set-faces! '(default :height 120))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13))

;; Setting up pyright

;; Enable lsp-mode for Python
(after! lsp-mode
  (setq lsp-pyright-multi-root nil)
  (setq lsp-pyright-auto-import-completions t))

(use-package! lsp-pyright
  :hook (python-mode . lsp-deferred))  ;; use lsp-deferred for better performance

;; Ensure company-mode is enabled
(use-package! company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

;; Enable flycheck globally for real-time syntax checking
(use-package! flycheck
  :hook (after-init . global-flycheck-mode))

;; Enable LSP diagnostics
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting t
        lsp-signature-auto-activate t
        lsp-ui-sideline-show-hover t
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor t))

;; Code execution
(defun execute-python-buffer ()
  "Execute the current buffer as Python code."
  (interactive)
  (let ((python-shell-interpreter "python3")) ; Change to "python" if using Python 2
    (python-shell-send-buffer)))

(defun execute-c-buffer ()
  "Compile and execute the current buffer as C code."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (unless (and file-name (string-match "\\.c$" file-name))
      (error "Not a C source file"))
    (let ((output-file-name (concat (file-name-sans-extension file-name) ".out")))
      (compile (format "gcc -o %s %s && ./ %s" output-file-name file-name output-file-name)))))
(map! :leader
      :desc "Execute Python Buffer"
      "e" #'execute-python-buffer)

(map! :leader
      :desc "Compile and Execute C Buffer"
      "c" #'execute-c-buffer)


(require 'dired)

;; Org mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(use-package org-superstar
  :ensure t
  :config
  ;; Define custom bullets for list items
  (setq org-superstar-item-bullet-alist '((?- . ?✦) (?+ . ?➤) (?* . ?•)))
  ;; Enable more compact leading bullets
  (setq org-superstar-leading-bullet "  ")
  ;; Hide the leading stars on headlines
  (setq org-superstar-leading-fallback ?\s)
  ;; Make leading bullets invisible
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.5 :weight bold))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.2 :weight bold))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 0.8))))
  '(org-level-5 ((t (:inherit outline-5 :height 0.6)))))

(after! org
  (setq org-directory "~/Org/"))

;; Org Agenda
(after! org
  (setq org-agenda-files '("~/Org/agenda.org")))

;; mu4e
(after! mu4e
  (setq mu4e-            "** Appointments\n"
            "- [ ] Appointment 1\n"
            "- [ ] Appointment 2\n\n"
            "** Notes\n"
            "- Note 1\n"
            "- Note 2\n\n"
maildir "~/Maildir"
        mu4e-sent-folder "/[Gmail]/Sent Mail"
        mu4e-drafts-folder "/[Gmail]/Drafts"
        mu4e-trash-folder "/[Gmail]/Trash"
        mu4e-refile-folder "/[Gmail]/All Mail"
        mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-use-fancy-chars t
        mu4e-headers-skip-duplicates t
        mu4e-headers-auto-update t
        mu4e-compose-dont-reply-to-self t)


  ;; Define contexts for each account
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Gmail1"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/gmail2004" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "shrishjay2004@gmail.com")
                  (user-full-name . "Shrishjay Acharya")
                  (mu4e-sent-folder . "/gmail2004/[Gmail]/Sent Mail")
                  (mu4e-drafts-folder . "/gmail2004/[Gmail]/Drafts")
                  (mu4e-trash-folder . "/gmail2004/[Gmail]/Trash")
                  (mu4e-refile-folder . "/gmail2004/[Gmail]/All Mail")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-user . "shrishjay2004@gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type . starttls)))

         (make-mu4e-context
          :name "GmailIIT"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/iitm" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "24f1000722@ds.study.iitm.ac.in")
                  (user-full-name . "Shrishjay Acharya")
                  (mu4e-sent-folder . "/iitm/[Gmail]/Sent Mail")
                  (mu4e-drafts-folder . "/iitm/[Gmail]/Drafts")
                  (mu4e-trash-folder . "/iitm/[Gmail]/Trash")
                  (mu4e-refile-folder . "/iitm/[Gmail]/All Mail")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-user . "24f1000722@ds.study.iitm.ac.in")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type . starttls)))))

  ;; Optional: Set default context (if desired)
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask)

  ;; Enable mu4e alert notifications
  (use-package! mu4e-alert
    :config
    (mu4e-alert-enable-mode-line-display)
    (mu4e-alert-set-default-style 'notifications))

  ;; Set up shortcuts for switching contexts (optional)
  (global-set-key (kbd "C-c m 1") (lambda () (interactive) (mu4e-context-switch nil "gmail2004")))
  (global-set-key (kbd "C-c m 2") (lambda () (interactive) (mu4e-context-switch nil "iitm"))))

;; Treemacs
;; ~/.doom.d/config.el

(use-package! treemacs
  :config
  (setq treemacs-width 35)  ; Set the width of the Treemacs window
  )

;; Optionally, you can customize other aspects of Treemacs
;; Bind a key to toggle Treemacs
(map! :leader
      :desc "Toggle Treemacs"
      "ft" #'treemacs)

