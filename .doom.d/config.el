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
;; Font
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15))
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
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
;; Dired
(use-package dired)
;; Company
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (add-to-list 'company-backends
                         '(company-capf :with company-yasnippet))))

(defun my/company-sort-snippet-first (candidates)
  (let ((snippets '())
        (others '()))
    (dolist (cand candidates)
      (if (get-text-property 0 'yas-annotation cand)
          (push cand snippets)
        (push cand others)))
    (append (nreverse snippets) (nreverse others))))

(setq company-transformers '(my/company-sort-snippet-first))

;; Debugging
(use-package dap-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)  ;; Set the debugger to debugpy
  ;; Register a new debug template
  (dap-register-debug-template "My App"
                               (list :type "python"
                                     :args "-i"
                                     :cwd nil
                                     :env '(("DEBUG" . "1"))
                                     :target-module (expand-file-name "~/src/myapp/.env/bin/myapp")
                                     :request "launch"
                                     :name "My App")))
;; Org mode
(setq org-agenda-files '("~/org/agenda/"))
(use-package org-superstar
  :ensure t
  :config
  ;; Define custom bullets for list items
  (setq org-superstar-item-bullet-alist '((?- . ?✦) (?+ . ?➤) (?* . ?•)))
  ;; Make leading bullets invisible
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
;; Size of the headings
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2 :weight bold)))))
(setq org-hide-emphasis-markers t)
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
;; Auto close other projects when opening a new one
; (treemacs-project-follow-mode 1)
;; Searching files by fd
(map! :leader
      :desc "Find files (fd)"
      "f z" #'consult-fd)

;; PDF
(after! pdf-tools
  (map! :map pdf-view-mode-map
        :n "C-=" #'pdf-view-enlarge
        :n "C--" #'pdf-view-shrink
        :n "C-0" #'pdf-view-scale-reset
        :n "n"   #'pdf-view-next-page
        :n "p"   #'pdf-view-previous-page))
