; clients, file templates and snippets. It is optional.
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
;; Font
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; in config.el
(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'mocha) ; or 'frappe 'latte, 'macchiato, or 'mocha
    (load-theme 'catppuccin t)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

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
;; Dashboard
;; (setq fancy-splash-image (concat doom-private-dir "splash.png"))
(setq initial-buffer-choice "~/.doom.d/start.org")

(define-minor-mode start-mode
  "Provide functions for custom start page."
  :lighter " start"
  :keymap (let ((map (make-sparse-keymap)))
          ;;(define-key map (kbd "M-z") 'eshell)
            (evil-define-key 'normal start-mode-map
              (kbd "1") '(lambda () (interactive) (find-file "~/.doom.d/config.el"))
              (kbd "2") '(lambda () (interactive) (find-file "~/.doom.d/init.el"))
              (kbd "3") '(lambda () (interactive) (find-file "~/.doom.d/packages.el")))
          map))

(add-hook 'start-mode-hook 'read-only-mode) ;; make start.org read-only; use 'SPC t r' to toggle off read-only.
(provide 'start-mode)
(add-hook 'emacs-startup-hook
  (lambda ()
    (message "Emacs loaded in %s."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done)))

;; Dired
(use-package dired)
;; integrate yasnippet-capf with eglot completion
;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
(defun mi/eglot-capf-with-yasnippet ()
  (setq-local completion-at-point-functions
              (list
	       (cape-capf-super
		#'eglot-completion-at-point
		#'yasnippet-capf))))
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'mi/eglot-capf-with-yasnippet))
;; Org mode
(setq org-agenda-files '("~/org/agenda/"))
(use-package org-superstar
  :ensure t
  :config
  ;; Define custom bullets for list items
  (setq org-superstar-item-bullet-alist '((?- . ?✦) (?+ . ?➤) (?* . ?•)))
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
;; side the special markers used for bold, italics and underline text
(setq org-hide-emphasis-markers t)
(after! org
  (custom-set-faces!
    '(org-table :foreground "cdd6f4" :weight normal))) ; use a brighter color

;; ein
(use-package! ein
  :config
  ;; Ensure that images display inline
  (setq ein:output-area-inlined-images t)
  (setq ein:use-auto-complete-superpack t) ;; Enable advanced completion
  (setq ein:worksheet-enable-undo t)     ;; Enable undo in notebooks
  ;; Automatically display images when cells are executed
  (add-hook 'ein:notebook-mode-hook
            (lambda ()
              (setq-local ein:output-area-inlined-images t)
              (setq ein:worksheet-enable-inline-images t))))
(setq ein:output-area-inlined-images-max-height 600)
;;Treemacs
(map! :leader
      :desc "Toggle Treemacs"
      "ft" #'treemacs)
(setq treemacs-show-hidden-files nil)
;; Searching files by fd
(defun my/consult-fd-from-home ()
  "Find file from home directory using consult-fd."
  (interactive)
  (let ((default-directory (expand-file-name "~")))
    (consult-fd)))
(map! :leader
      :desc "Find file from ~"
      "f z" #'my/consult-fd-from-home)

;; PDF
(after! pdf-tools
  (map! :map pdf-view-mode-map
        :n "C-=" #'pdf-view-enlarge
        :n "C--" #'pdf-view-shrink
        :n "C-0" #'pdf-view-scale-reset
        :n "n"   #'pdf-view-next-page
        :n "p"   #'pdf-view-previous-page))
;; Tree-sitter
(use-package! tree-sitter
  :hook (python-mode . tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package! tree-sitter-langs
  :after tree-sitter)
