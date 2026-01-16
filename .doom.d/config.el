;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 15))
(custom-set-faces!
  '(default :background "#16181c")
  '(solaire-default-face :background "#16181c")) 
(setq display-line-numbers-type 'relative)
(setq fancy-splash-image (concat doom-private-dir "media/splash.png"))
(use-package dired)
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq-default header-line-format " ")

;; Define the function first (always available)
(after! corfu
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.05))
(defun mi/eglot-capf-with-yasnippet ()
 (setq-local completion-at-point-functions
             (list
	       (cape-capf-super
		#'eglot-completion-at-point
		#'yasnippet-capf))))
(with-eval-after-load 'eglot
 (add-hook 'eglot-managed-mode-hook #'mi/eglot-capf-with-yasnippet))
(map! :leader
      :desc "Enable Eglot in buffer"
      "e" #'eglot)
(add-hook 'python-ts-mode-hook 
          (lambda ()
            (run-with-timer 0.0 nil #'eglot-ensure)))
(add-hook 'inferior-python-mode-hook
          (lambda ()
            (corfu-mode -1)))

(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/agenda/"))
(setq org-clock-sound "~/.doom.d/media/beep.wav")
(after! org
  (custom-set-faces!
    '(org-table :foreground "cdd6f4" :weight normal))) ; use a brighter color

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
(map! :leader
      :desc "Find file from directory"
      :"f a" #'consult-fd)

(after! pdf-tools
  (map! :map pdf-view-mode-map
        :n "C-=" #'pdf-view-enlarge
        :n "C--" #'pdf-view-shrink
        :n "C-0" #'pdf-view-scale-reset
        :n "n"   #'pdf-view-next-page
        :n "p"   #'pdf-view-previous-page))

(use-package! treesit
  :config
  ;; Automatically use tree-sitter modes when available
  (setq treesit-font-lock-level 4) ;; Maximum highlighting detail
  
  ;; Remap major modes to their tree-sitter variants
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode))))

(map! :leader
      :desc "Open vterm"
      "v t" #'vterm)
