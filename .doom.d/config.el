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
(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'mocha) ;; or 'latte', 'frappe', 'macchiato'

(custom-set-faces!
  '(font-lock-comment-face :foreground "#6D6D6D"))

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
;; Org-bullets

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Org-superstar
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
;; Size of the headings
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.5 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2 :weight bold))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 0.8))))
 '(org-level-5 ((t (:inherit outline-5 :height 0.6)))))

(after! org
  (setq org-directory "~/Org/")
  (setq org-agenda-files '("~/Org/")))
;; Enable org-pretty-entities for better math formatting
(setq org-pretty-entities t)

;; Ensure scheduled items appear in the agenda
(setq org-agenda-span 'week) ;; or 'day, 'month, etc.
;; Org roam
;; mu4e
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))
(require 'org-mime)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")

(use-package org
  :ensure t)
(require 'mu4e)

(setq mu4e-maildir (expand-file-name "~/Maildir"))

                                        ; get mail
(setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
      ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
      mu4e-view-prefer-html t
      mu4e-update-interval 180
      mu4e-headers-auto-update t
      mu4e-compose-signature-auto-include nil
      mu4e-compose-format-flowed t)

;; to view selected message in the browser, no signin, just html mail
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; every new email composition gets its own frame!
(setq mu4e-compose-in-new-frame t)

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

(add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;; <tab> to navigate to links, <RET> to open them in browser
(add-hook 'mu4e-view-mode-hook
          (lambda()
            ;; try to emulate some of the eww key-bindings
            (local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
            (local-set-key (kbd "<tab>") 'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
(add-hook 'mu4e-headers-mode-hook
          (defun my/mu4e-change-headers ()
	    (interactive)
	    (setq mu4e-headers-fields
	          `((:human-date . 25) ;; alternatively, use :date
		    (:flags . 6)
		    (:from . 22)
		    (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
		    (:size . 7)))))

;; if you use date instead of human-date in the above, use this setting
;; give me ISO(ish) format date-time stamps in the header list
                                        ;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

;; spell check
(add-hook 'mu4e-compose-mode-hook
          (defun my-do-compose-stuff ()
            "My settings for message composition."
            (visual-line-mode)
            (use-hard-newlines -1)
            (flyspell-mode)))

(require 'smtpmail)

;;rename files when moving
;;NEEDED FOR MBSYNC
(setq mu4e-change-filenames-when-moving t)

;;set up queue for offline email
;;use mu mkdir  ~/Maildir/acc/queue to set up first
(setq smtpmail-queue-mail nil)  ;; start in normal mode

;;from the info manual
(setq mu4e-attachment-dir  "~/Downloads")

(setq message-kill-buffer-on-exit t)
(setq mu4e-compose-dont-reply-to-self t)


;; convert org mode to HTML automatically

;;from vxlabs config
;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; don't ask when quitting
(setq mu4e-confirm-quit nil)

;; mu4e-context
(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy 'always-ask)
(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "work" ;;for shrishjay2004
        :enter-func (lambda () (mu4e-message "Entering context work"))
        :leave-func (lambda () (mu4e-message "Leaving context work"))
        :match-func (lambda (msg)
		      (when msg
		        (mu4e-message-contact-field-matches
		         msg '(:from :to :cc :bcc) "shrishjay2004@gmail.com")))
        :vars '((user-mail-address . "shrishjay2004@gmail.com")
	        (user-full-name . "Shrishjay Acharya")
	        (mu4e-sent-folder . "/shrishjay2004/[shrishjay2004].Sent Mail")
	        (mu4e-drafts-folder . "/shrishjay2004/[shrishjay2004].drafts")
	        (mu4e-trash-folder . "/shrishjay2004/[shrishjay2004].Trash")
	        (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 25, org-mode 9, mu4e 1.0\n"))
	        (mu4e-compose-format-flowed . t)
	        (smtpmail-queue-dir . "~/Maildir/shrishjay2004/queue/cur")
	        (message-send-mail-function . smtpmail-send-it)
	        (smtpmail-smtp-user . "shrishjay2004")
	        (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                                        ;(smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
	        (smtpmail-default-smtp-server . "smtp.gmail.com")
	        (smtpmail-smtp-server . "smtp.gmail.com")
	        (smtpmail-smtp-service . 587)
	        (smtpmail-debug-info . t)
	        (smtpmail-debug-verbose . t)
	        (mu4e-maildir-shortcuts . ( ("/shrishjay2004/INBOX"            . ?i)
					    ("/shrishjay2004/[shrishjay2004].Sent Mail" . ?s)
					    ("/shrishjay2004/[shrishjay2004].Bin"       . ?t)
					    ("/shrishjay2004/[shrishjay2004].All Mail"  . ?a)
					    ("/shrishjay2004/[shrishjay2004].Starred"   . ?r)
					    ("/shrishjay2004/[shrishjay2004].drafts"    . ?d)
					    ))))
       (make-mu4e-context
        :name "personal" ;;for iitm
        :enter-func (lambda () (mu4e-message "Entering context personal"))
        :leave-func (lambda () (mu4e-message "Leaving context personal"))
        :match-func (lambda (msg)
		      (when msg
		        (mu4e-message-contact-field-matches
		         msg '(:from :to :cc :bcc) "iitm@gmail.com")))
        :vars '((user-mail-address . "iitm@gmail.com")
	        (user-full-name . "Shrishjay Acharya(iitm)")
	        (mu4e-sent-folder . "/iitm/[iitm].Sent Mail")
	        (mu4e-drafts-folder . "/iitm/[iitm].drafts")
	        (mu4e-trash-folder . "/iitm/[iitm].Trash")
	        (mu4e-compose-signature . (concat "Informal Signature\n" "Emacs is awesome!\n"))
	        (mu4e-compose-format-flowed . t)
	        (smtpmail-queue-dir . "~/Maildir/iitm/queue/cur")
	        (message-send-mail-function . smtpmail-send-it)
	        (smtpmail-smtp-user . "iitm")
	        (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                                        ;(smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
	        (smtpmail-default-smtp-server . "smtp.gmail.com")
	        (smtpmail-smtp-server . "smtp.gmail.com")
	        (smtpmail-smtp-service . 587)
	        (smtpmail-debug-info . t)
	        (smtpmail-debug-verbose . t)
	        (mu4e-maildir-shortcuts . ( ("/iitm/INBOX"            . ?i)
					    ("/iitm/[iitm].Sent Mail" . ?s)
					    ("/iitm/[iitm].Trash"     . ?t)
					    ("/iitm/[iitm].All Mail"  . ?a)
					    ("/iitm/[iitm].Starred"   . ?r)
					    ("/iitm/[iitm].drafts"    . ?d)
					    ))))))
;; Treemacs
;; ~/.doom.d/config.el

(use-package! treemacs
  :config
  (setq treemacs-width 35)  ; Set the width of the Treemacs window
  )
(map! :leader
      :desc "Toggle Treemacs"
      "ft" #'treemacs)

;; Formatting code using format all
;; Enable format-all-mode for all programming modes
(use-package! format-all
  :hook (prog-mode . format-all-mode))


;; Optional: Automatically format buffer on save
(use-package! format-all
  :hook (prog-mode . format-all-ensure-formatter)
  :config
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  (setq format-all-format-on-save-mode t))

;; Ensure the `jupyter` package is loaded
(use-package! jupyter
  :defer t
  :init
  (setq jupyter-repl-echo-eval-p t)
  :config
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")
                                                       (:kernel . "python3"))))

;; Enable org-mode to handle .ipynb files
(add-to-list 'auto-mode-alist '("\\.ipynb\\'" . org-mode))

;; Configure org-mode to use Jupyter
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (jupyter . t)))

;; Optional: Keybindings for ease of use
(map! :leader
      (:prefix ("j" . "jupyter")
       :desc "Run cell" "r" #'jupyter-org-execute-src-block
       :desc "Interrupt kernel" "i" #'jupyter-repl-interrupt-kernel
       :desc "Restart kernel" "k" #'jupyter-repl-restart-kernel
       :desc "Open console" "c" #'jupyter-repl-pop-to-buffer))

;; Adding code runner feature in vterm for python
;; (defun run-python-file-in-vterm ()
;;   "Run the current python file in vterm."
;;   (interactive)
;;   (let ((file (buffer-file-name)))
;;     (vterm)
;;     (vterm-send-string (format "python3 %s\n" file))))

;; (map! :leader
;;       :desc "Run Python file in vterm"
;;       "m r" #'run-python-file-in-vterm)

;; Java
(use-package! lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or whatever prefix you prefer
  :config
  (lsp-enable-which-key-integration t))

(use-package! lsp-java
  :after lsp
  :config
  (add-hook 'java-mode-hook #'lsp))

(require 'dap-mode)

;; Enable DAP mode
(dap-mode 1)
(dap-ui-mode 1)

;; Python specific configuration
(require 'dap-python)
(setq dap-python-executable "python") ; Set this to your Python executable if different
                                        ;(use-package! centaur-tabs
;; Load and configure centaur-tabs
(use-package! centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  ;; Set the style and appearance of the tabs
  (setq centaur-tabs-set-bar 'under
        centaur-tabs-set-icons t
        centaur-tabs-set-modified-marker t
        centaur-tabs-show-navigation-buttons t
        centaur-tabs-style "bar"
        centaur-tabs-height 32
        centaur-tabs-enable-key-bindings t
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-gray-out-icons 'buffer))
;;   ;; Enable cycling through tabs with keybindings
(global-set-key (kbd "C-<tab>") 'centaur-tabs-forward)
(global-set-key (kbd "C-S-<tab>") 'centaur-tabs-backward)

;; Enabling sticky scrolling
;; Enable Semantic
(use-package! semantic
  :config
  (semantic-mode 1) ;; Enable Semantic mode globally
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-idle-completions-mode 1)
  (global-semantic-idle-summary-mode 1)
  (global-semantic-decoration-mode 1)
  (global-semantic-highlight-func-mode 1)
  (global-semantic-mru-bookmark-mode 1)
  ;; Enable Sticky Function mode globally
  (global-semantic-stickyfunc-mode 1)
  ;; Optional: Customize the sticky classes
  (setq semantic-stickyfunc-sticky-classes '(function type))
  ;; Optional: Customize header line face for better visibility
  (custom-set-faces
   '(semantic-stickyfunc-indicator-face ((t (:background "gray20" :foreground "gold"))))))
;; (use-package! vterm
;;   :commands vterm
;;   :config
;;   (setq vterm-max-scrollback 10000))

(defun my/run-code-in-vterm ()
  "Run the current buffer's code in a vterm."
  (interactive)
  (let* ((file (buffer-file-name))
         (buf (get-buffer-create "*vterm-run*"))
         (cmd (cond
               ((string-match "\\.py\\'" file) (format "python %s" file))
               ((string-match "\\.c\\'" file) (format "gcc %s -o %s && ./%s" file (file-name-sans-extension file) (file-name-sans-extension file)))
               ((string-match "\\.java\\'" file) (format "javac %s && java %s" file (file-name-sans-extension file)))
               (t (read-string "Run command: ")))))
    (with-current-buffer buf
      (vterm-mode)
      (vterm-send-string cmd)
      (vterm-send-return))
    (display-buffer buf)))

(map! :leader
      :desc "Run code in vterm" "r" #'my/run-code-in-vterm)

;; MySQL
;; SQL mode configuration
(use-package! sql
  :config
  ;; Set default SQL program to MySQL
  (setq sql-program "mysql")
  (setq sql-product 'mysql)

  ;; Define SQL connection settings
  (setq sql-connection-alist
        '((mysql (sql-product 'mysql)
           (sql-login "root")
           (sql-password "golimaar")  ;; Replace with your actual password
           (sql-database "shrishjay_data")
           (sql-port 3306)
           (sql-server "localhost"))))

  ;; Additional configurations if needed
  (setq sql-mysql-program "mysql"))

;; Add keybindings for multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
