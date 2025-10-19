;; -*- lexical-binding: t -*-
(require 'package)
(package-initialize)

;;
;; Package management
;;
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq straight-use-package-by-default t
      straight-repository-branch "master"
      ;; single file for caching autoloads
      straight-cache-autoloads t
      ;; NOTE: requires python3 and watchexec
      ;; straight-check-for-modifications '(watch-files find-when-checking)
      ;; NOTE: requires no watchexec
      straight-find-executable "fd"
      straight-check-for-modifications '(check-on-save find-when-checking)
      )

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(save-place-mode 1)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

(setq-default indent-tabs-mode nil)
(setq-default dabbrev-case-fold-search nil)
(setq-default fill-column 120)

;; Do not show native compilation warnings
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;; Delete the selected text upon text insertion
(use-package delsel
  :ensure nil ; no need to install it as it is built-in
  :straight (:type built-in)
  :hook (after-init . delete-selection-mode))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; Fonts
(if (string-equal (system-name) "dimsuzkode")
    (set-face-attribute 'default nil :font "Iosevka" :height 110)
  (set-face-attribute 'default nil :font "Jetbrains Mono" :height 130))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun dz/switch-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;; Global key bindings
(global-set-key (kbd "C-c f i") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-c C-f i") (lambda () (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-`") #'dz/switch-other-buffer)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c C-i") #'imenu)
(global-set-key (kbd "C-c ,") #'consult-buffer)
(global-set-key (kbd "C-c f .") (lambda () (interactive) (dired ".")))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(setq display-buffer-alist
      '(("\\*compilation\\*"
         (display-buffer-reuse-window display-buffer-use-some-window display-buffer-in-direction)
         (direction . right)
         (inhibit-same-window . t))))

(setq compilation-ask-about-save nil)
(setq compile-command "./run dm")

(use-package cc-mode
  :config
  (define-key c-mode-map (kbd "C-c C-c") #'recompile)
  (define-key c-mode-map (kbd "C-c C-S-c") #'compile))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;
;; Undo
;;
;; TODO switch to vundo
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  ;; Prevent undo tree files from polluting your git repo
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(defvar use-evil-or-meow "meow")

(use-package magit
  :commands magit-status
  :bind
  (:map global-map
        ("C-M-g" . magit-status))
  :hook
  (magit-status-mode . (lambda () (whitespace-mode -1)))
  :config
  (if (string-equal use-evil-or-meow "evil")
      (add-hook 'magit-status-mode-hook (lambda () (god-local-mode -1))))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;
;; Keybindings
;;
(if (string-equal use-evil-or-meow "evil")
    (progn (use-package general
	     :after evil
	     :config
	     (general-create-definer dz/leader-keys
	       :keymaps '(normal emacs)
	       :prefix "SPC"
	       )
	     (dz/leader-keys
	      "t"  '(:ignore t :which-key "toggles")
	      "tt" '(counsel-load-theme :which-key "choose theme")
	      "b"  '(:ignore t :which-key "buffers")
	      "bk" '(kill-current-buffer :which-key "kill buffer")
	      "," '(switch-to-buffer :which-key "switch buffer")
	      )
	     (general-define-key
	      :prefix "SPC"
	      :states 'normal
	      :keymaps 'override
	      "p" '(:keymap projectile-command-map :package projectile :which-key "projectile prefix")
	      ))
	   (use-package evil
	     :init
	     (setq evil-want-integration t)
	     (setq evil-want-keybinding nil)
	     (setq evil-want-C-u-scroll t)
	     (setq evil-want-C-d-scroll t)
	     (setq evil-want-C-i-jump nil)
	     :config
	     (evil-mode 1)
	     (evil-set-undo-system 'undo-tree)
	     (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
	     (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
	     (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
	     ;; Use visual line motions even outside of visual-line-mode buffers
	     (evil-global-set-key 'motion "j" 'evil-next-visual-line)
	     (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

	     (evil-set-initial-state 'messages-buffer-mode 'normal)
	     (evil-set-initial-state 'dashboard-mode 'normal))

	   (use-package evil-collection
	     :after evil
	     :config
	     (evil-collection-init))
	   ))

;;
;; Meow mode
;;
(defun meow-setup ()
  ;; free this key (opens FAQ by default) to be easier to run C-h f from meow-keypad
  (global-unset-key (kbd "C-h C-f"))
  ;; free this key (opens FAQ by default) to be easier to run C-h f from meow-keypad
  (global-unset-key (kbd "C-x C-p"))

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("h" . meow-left)
   '("l" . meow-right)
   '("`" . dz/switch-other-buffer)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("o" . ace-window))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("=" . meow-indent)
   '("<escape>" . ignore)
   '("`" . dz/switch-other-buffer)))

(use-package meow
  :ensure t
  :if (string-equal use-evil-or-meow "meow")
  :straight (:host github :repo "meow-edit/meow" :branch "master")
  :config
  (meow-setup)
  (meow-global-mode 1))

(defun dz/god-mode-update-cursor-type ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

(use-package god-mode
  :ensure t
  :if (string-equal use-evil-or-meow "god-mode")
  :config
  (global-set-key (kbd "<escape>") #'god-mode-all)
  (define-key god-local-mode-map (kbd "i") #'god-local-mode)
  ;; (global-set-key (kbd "<escape>") #'(lambda () (interactive) (god-local-mode 1)))
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil)
  (define-key god-local-mode-map (kbd ".") #'repeat)
  (global-set-key (kbd "C-x C-1") #'delete-other-windows)
  (global-set-key (kbd "C-x C-2") #'split-window-right)
  (global-set-key (kbd "C-x C-3") #'split-window-below)
  (global-set-key (kbd "C-x C-0") #'delete-window)
  ;; using ace-window instead
  ;; (global-set-key (kbd "C-x C-o") #'other-window)
  (define-key god-local-mode-map (kbd "[") #'backward-paragraph)
  (define-key god-local-mode-map (kbd "]") #'forward-paragraph)
  (add-hook 'god-mode-enabled-hook #'dz/god-mode-update-cursor-type)
  (add-hook 'god-mode-disabled-hook #'dz/god-mode-update-cursor-type))

(use-package yasnippet
  :straight nil
  :load-path "plugins/yasnippet"
  :config
  (yas-global-mode 1))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x C-o") #'ace-window)
  (setq aw-scope 'frame))

;;
;; Themes
;;
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; commented: using another one
  ;; (load-theme 'doom-solarized-light t)

  ;; Enable flashing mode-line on errors
  ;;(doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package modus-themes
  :ensure t)

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-spring :no-confirm))

;; Delimeters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;
;; Helpful
;;
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;;
;; Org
;;
(defun dz/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Jetbrains Mono" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun dz/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :straight (org :host github
                 :repo "emacs-straight/org-mode"
                 :local-repo "org"
                 :depth 1)
  :commands (org-capture org-agenda)
  :hook (org-mode . dz/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-return-follows-link t)

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (dz/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun dz/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dz/org-mode-visual-fill))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/org-roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
)

;;
;; Modeline
;;
;; TODO customize modeline
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Which key
(use-package which-key
  :diminish which-key-mode
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;;
;; Vertico
;;
(use-package vertico
  :init
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(defun dz/consult-ripgrep ()
  (interactive)
  (consult-ripgrep nil (thing-at-point 'symbol)))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x C-b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
          ;;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("C-c s r" . dz/consult-ripgrep)
         ("M-s l" . consult-line)
         ;; search integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package savehist
  :ensure nil ; it is built-in
  :straight (:type built-in)
  :hook (after-init . savehist-mode))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package dired
  :ensure nil
  :straight (:type built-in)
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode)
   (dired-mode . (lambda () (whitespace-mode -1))))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
	("<tab>" . dired-subtree-toggle)
	("TAB" . dired-subtree-toggle)
	("<backtab>" . dired-subtree-remove)
	("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package project
  :ensure nil
  :straight (:type built-in)
  :config
  (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m)))

;; (use-package format-all
;;   :hook ((format-all-mode . format-all-ensure-formatter)
;; 	 (prog-mode . format-all-mode)))

(use-package odin-mode
  :ensure t
  :straight (:host github :repo "mattt-b/odin-mode")
  :config
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2)
  ;; this enables 'gc' to use the correct comment style
  (setq comment-start "// "
        comment-end "")
  (modify-syntax-entry ?_ "w"))

(use-package kotlin-mode
  :ensure t
  :config
  (setq kotlin-tab-width 4)
  (setq kotlin-mode-parenthesized-expression-offset 4)
  (setq kotlin-mode-multiline-statement-offset 4)
  )

(add-hook 'whitespace-mode-hook
          (lambda ()
            (setq whitespace-style '(face tabs trailing lines tab-mark))))
(global-whitespace-mode 1)

;;(use-package whitespace-mode
;;  :config
;;  (setq whitespace-line-column 120)
;;  ;; (setq whitespace-style '(face trailing lines tabs))
;;  )
