(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15))
(setq doom-theme 'catppuccin)
(setq catppuccin-flavor 'mocha) ; or 'frappe 'latte, 'macchiato, or 'mocha
    (load-theme 'catppuccin t)
(setq display-line-numbers-type 'relative)

(setq fancy-splash-image (concat doom-private-dir "splash.png"))

(defun mi/eglot-capf-with-yasnippet ()
  (setq-local completion-at-point-functions
              (list
	       (cape-capf-super
		#'eglot-completion-at-point
		#'yasnippet-capf))))
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook #'mi/eglot-capf-with-yasnippet))

(setq org-directory "~/org/")
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

(map! :leader
      :desc "Toggle Treemacs"
      "ft" #'treemacs)
(setq treemacs-show-hidden-files nil)

(defun my/consult-fd-from-home ()
  "Find file from home directory using consult-fd."
  (interactive)
  (let ((default-directory (expand-file-name "~")))
    (consult-fd)))
(map! :leader
      :desc "Find file from ~"
      "f z" #'my/consult-fd-from-home)

(after! pdf-tools
  (map! :map pdf-view-mode-map
        :n "C-=" #'pdf-view-enlarge
        :n "C--" #'pdf-view-shrink
        :n "C-0" #'pdf-view-scale-reset
        :n "n"   #'pdf-view-next-page
        :n "p"   #'pdf-view-previous-page))

(use-package! tree-sitter
  :hook (python-mode . tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package! tree-sitter-langs
  :after tree-sitter)
