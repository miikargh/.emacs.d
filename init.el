;; NOTE: init.el is generated from init.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun miika/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'miika/display-startup-time)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
      ("org" . "https://orgmode.org/elpa/")
      ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
(package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Uncomment the following for debugging emacs startup
;; (setq use-package-verbose t)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 5) ;; Padding on sides
(menu-bar-mode -1)

(show-paren-mode 1)



(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)


;; Disable line numbers from some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Iosevka" :height 140 :weight 'light)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; NOTE: The first time you load your configuration on a new machine, you'll
;; need to run the following command interactively so that mode line icons
;; display correctly:
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package nyan-mode
  :init (nyan-mode t)
  :config
  (setq nyan-animate-nyancat t
        nyan-wavy-trail t))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; Mac stuff
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)


  (defun miika/open-user-init-file ()
    "Edit emacs config, in another window."
    (interactive)
    (find-file (expand-file-name "~/.emacs.d/init.org")))


  ;; todo highlighting
  (use-package hl-todo
  :config (hl-todo-mode))


  (add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package command-log-mode
  :commands command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("M-j" . ivy-next-line)
         ("M-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  :init (ivy-mode 1))



(use-package which-key
  :defer 0
  :diminish wich-key-mode
  :config
    (setq which-key-idle-delay 0.3)
    (which-key-mode))

(use-package ivy-rich
  :after ivy
  :config (ivy-rich-mode 1))

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defun miika/focus-next-window-or-open-new ()
  "Move focus to the next window or opens a new window if only one is open."
  (interactive)
  (when (one-window-p)
    (evil-window-vsplit))
  (evil-window-next nil))

(defun miika/visual-shift-left ()
  "Make shifting not loose focus"
  (interactive)
  (call-interactively 'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(defun miika/visual-shift-right ()
  "Make shifting not loose focus"
  (interactive)
  (call-interactively 'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))



(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "ä" ) 'evil-backward-paragraph)
  (define-key evil-visual-state-map (kbd "ä" ) 'evil-backward-paragraph)
  (define-key evil-normal-state-map (kbd "ö" ) 'evil-forward-paragraph)
  (define-key evil-visual-state-map (kbd "ö" ) 'evil-forward-paragraph)
  (define-key evil-normal-state-map (kbd "å") 'evil-first-non-blank)
  (define-key evil-visual-state-map (kbd "å") 'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "Å") 'evil-last-non-blank)
  (define-key evil-visual-state-map (kbd "Å") 'evil-last-non-blank)
  (define-key evil-visual-state-map (kbd ">") 'miika/visual-shift-right)
  (define-key evil-visual-state-map (kbd "<") 'miika/visual-shift-left)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package smartparens
  :after evil
  :config
  (smartparens-global-mode t)
  (sp-pair "'" nil :actions :rem))

(use-package evil-smartparens
  :after (smartparens)
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package evil-commentary
  :after evil
  :init (evil-commentary-mode))

(use-package evil-snipe
  :config
    (evil-snipe-mode +1)
    (evil-snipe-override-mode +1)
    (evil-define-key 'visual evil-snipe-local-mode-map "z" 'evil-snipe-s)
    (evil-define-key 'visual evil-snipe-local-mode-map "Z" 'evil-snipe-S))

(use-package evil-multiedit
  :config (evil-multiedit-default-keybinds))

(use-package evil-easymotion)

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package general
  :config

  (general-define-key
    :states 'insert
    :keymaps 'override
    "M-j" 'company-select-next
    "M-k" 'company-select-previous)

  (general-create-definer miika/leader-keys
    ;; :keymaps '(normal visual emacs)
    :states '(normal visual emacs)
    :prefix "SPC")

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'override
   "/" '(swiper :which-key "swiper"))

  (miika/leader-keys
    ":" '(counsel-M-x :which-key "M-x")
    ";" '(eval-expression :which-key "Eval expression")
    "." '(projectile-find-file :which-key "Find file in project")
    "SPC" '(:keymap evilem-map :which-key "Easy motion")
    "SPC s" '(evil-avy-goto-char
              :keymaps: 'override)
    "SPC S" '(evil-avy-goto-char-2
              :keymaps: 'override)

    ;; "/" '(swiper :which-key "swiper")

    "x" '(:keymap ctl-x-map :which-key "C-x")
    "c" '(:keymap mode-specific-map :which-key "C-c")
    "h" '(:keymap help-map :which-key "Help")

    ;; Buffers
    "b" '(:ignore t :which-key "Buffer")
    "bb" '(counsel-switch-buffer :which-key "Switch to buffer")
    "bv" '(miika/switch-to-vterm-buffer :which-key "Switch to vterm buffer")
    "bk" '(kill-current-buffer :which-key "Kill current buffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "Next buffer")

    ;; Text and themes
    "t" '(:ignore t :which-key "Toggle")
    "tt" '(counsel-load-theme :which-key "Load theme")
    "ts" '(hydra-text-scale/body :which-key "Scale text")

    ;; Window management
    "w" '(:keymap evil-window-map :package evil)
    "ww" '(miika/focus-next-window-or-open-new
           :keymaps 'override
           :which-key "Focus on next window or open new")

    ;; Files
    "f" '(:ignore t :which-key "File")
    "fi" '(miika/open-user-init-file :which-key "Open init.el")
    "ff" '(find-file :which-key "Find file")
    "f ." '(projectile-find-file-in-directory :which-key "Find file in dir")

    ;; Mode stuff
    "m" '(:ignore t :which-key "Mode")
    "mf" '(:ignore t :which-key "Format")

    ;; Magit
    "g" '(:ignore t :which-key "Magit")
    "gg" '(magit-status :which-key "Git status")
    "gb" '(magit-branch :which-key "Git branch")
    "gF" '(magit-fetch :which-key "Git pull")

    ;; Projects
    "p" '(:keymap projectile-command-map :package projectile)

    ;; UI
    "u" '(:ignore t :which-key "UI")

    ;; Terminal
    "i" '(:ignore t :which-key "Terminal")
    "ii" '(miika/multi-vterm-dedicated-toggle :which-key "Toggle dedicated vterm")
    "it" '(miika/multi-vterm :which-key "Open new vterm")
    "io" '(multi-vterm-next :which-key "Next vterm")
    "iu" '(multi-vterm-prev :which-key "Prev vterm")
    ))

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  ;; :bind-keymap
  ;; ("SPC p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev")))
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (setq projectile-globally-ignored-directories (append '(".bloop" ".bsp" ".metals" "target") projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-files (append '(".#*" "#*") projectile-globally-ignored-files))
  )

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package flycheck
  :defer t
  :config (global-flycheck-mode))

(defun miika/company-complete-selection ()
  "Insert the selected candidate or the first if none are selected.
    From: https://www.reddit.com/r/emacs/comments/kmeuft/companymode_not_autocompleting_first_candidate/"
  (interactive)
  (if company-selection
      (company-complete-selection)
    (company-complete-number 1)))

(use-package company
  ;; :after (lsp-mode emacs-lisp-mode)
  :hook (emacs-lisp-mode . company-mode)
  ;; :init
  ;; (add-hook 'emacs-lisp-mode-hook 'company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . miika/company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; Nicer UI
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :commands (lsp lsp-deferred)
  :hook
  (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :init
  (setq lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-keymap-prefix "C-c l")

  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000) ;; 100mb
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (miika/leader-keys
    :keymap lsp-mode-map
    "mfa" '(lsp-format-buffer :which-key "Format buffer")
    "mfr" '(lsp-format-region :which-key "Format region")
    "ud" '(miika/toggle-lsp-ui-doc :which-key "Toggle lsp-ui-doc")
    "r" '(:ignore t :which-key "Refactor")
    "rr" '(lsp-rename :which-key "Rename symbol")))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.0
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-sideline-show-diagnostics t))


(defun miika/toggle-lsp-ui-doc ()
  "Show lsp-ui-doc if if it is hidden and hides if not."
  (interactive)
  (if (lsp-ui-doc--visible-p)
      (lsp-ui-doc-hide)
    (lsp-ui-doc-show)))

(use-package company-lsp
  :after comapny-mode
  :config
  (setq company-lsp-cache-candidates 'auto))

(use-package posframe
  :after lsp-ui)

(use-package dap-mode
    :commands dap-debug
    :hook
    (lsp-mode . dap-mode)
    (lsp-mode . dap-ui-mode))

(miika/leader-keys
  :keymaps 'emacs-lisp-mode-map
  :states '(normal visual)
  ;; Eval
  "e" '(:ignore t :which-key "Eval")
  "ed" '(eval-defun :which-key "Eval defun")
  "er" '(eval-region :which-key "Eval region")
  "eb" '(eval-region :which-key "Eval buffer"))

(use-package scala-mode
  :mode "\\.scala\\'"
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :after scala-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package lsp-metals
  :after scala-mode
  :config
  (setq lsp-metals-treeview-show-when-views-received nil))

(use-package conda
  :config
    (custom-set-variables
    '(conda-anaconda-home (expand-file-name "~/miniconda3/")))
    (setq conda-env-home-directory (expand-file-name "~/miniconda3/"))
    (conda-env-initialize-interactive-shells)
    (conda-env-autoactivate-mode t)
    (add-to-list 'global-mode-string
                '(conda-env-current-name (" conda:" conda-env-current-name " "))
                'append)
    (conda-env-initialize-eshell)
   :after (eshell))

(use-package magit
  :commands magit-status
  :config
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package ediff
  :after magit
  :config
  (setq ediff-split-window-function 'split-window-horizontally))

(defun miika/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . miika/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  ;; (eshell-git-prompt-use-theme 'powerline)
)

(defun eshell-exec-in-vterm (&rest args)
  "https://git.jeremydormitzer.com/jdormit/dotfiles/commit/b7c4e383a2a3d8a0140376e9ebb76a3b7897848a"
    (let* ((program (car args))
            (buf (generate-new-buffer
                    (concat "*" (file-name-nondirectory program) "*"))))
        (with-current-buffer buf
        (vterm-mode)
        (vterm-send-string (concat (s-join " " args) "\n")))
        (switch-to-buffer buf)))

(use-package multi-vterm)

(use-package vterm
  :after (multi-vterm)
  :commands (vterm vterm-other-window vterm-mode)
  :config
  (with-eval-after-load 'em-term
    (defun eshell-exec-visual (&rest args)
      (apply #'eshell-exec-in-vterm args)))
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(defun miika/switch-to-vterm-buffer ()
  "Switch to a vterm buffer, or create one."
  (interactive)
  (ivy-read "Vterm buffer: " (counsel--buffers-with-mode #'vterm-mode)
            :action #'miika/switch-to-vterm
            :caller 'miika/switch-to-vterm-buffer))

(defun miika/switch-to-vterm (name)
  "Display vterm buffer with NAME and select its window.
Reuse any existing window already displaying the named buffer.
If there is no such buffer, start a new `vterm' with NAME."
  (if (get-buffer name)
      (pop-to-buffer name '((display-buffer-reuse-window
                             display-buffer-same-window)
                            (inhibit-same-window . nil)
                            (reusable-frames . visible)))
    (let ((default-directory (miika/get-project-root-dir)))
      (vterm name))))

(defun miika/multi-vterm ()
  "Create new vterm buffer but open in project root if possible."
  (interactive)
  (let* ((default-directory (miika/get-project-root-dir))
         (vterm-buffer (multi-vterm-get-buffer)))
    (setq multi-vterm-buffer-list (nconc multi-vterm-buffer-list (list vterm-buffer)))
    (set-buffer vterm-buffer)
    (multi-vterm-internal)
    (switch-to-buffer vterm-buffer)))

(defun miika/get-project-root-dir ()
  "Get the root directory of the current project if available."
    (project-root
     (or (project-current) `(transient . ,default-directory))))

(defun miika/multi-vterm-dedicated-toggle ()
  "Toggle dedicated `multi-vterm' window but in project root."
  (interactive)
  (if (multi-vterm-dedicated-exist-p)
      (multi-vterm-dedicated-close)
    (miika/multi-vterm-dedicated-open)))

(defun miika/multi-vterm-dedicated-open ()
  "Open dedicated `multi-vterm' window but in project root."
  (interactive)
  (if (not (multi-vterm-dedicated-exist-p))
      (if (multi-vterm-buffer-exist-p multi-vterm-dedicated-buffer)
          (unless (multi-vterm-window-exist-p multi-vterm-dedicated-window)
            (multi-vterm-dedicated-get-window))
        (let ((default-directory (miika/get-project-root-dir)))
          (setq multi-vterm-dedicated-buffer (multi-vterm-get-buffer 'dedicated)))
        (set-buffer (multi-vterm-dedicated-get-buffer-name))
        (multi-vterm-dedicated-get-window)
        (multi-vterm-internal)))
  (set-window-buffer multi-vterm-dedicated-window (get-buffer (multi-vterm-dedicated-get-buffer-name)))
  (set-window-dedicated-p multi-vterm-dedicated-window t)
  (select-window multi-vterm-dedicated-window)
  (message "`multi-vterm' dedicated window has exist."))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Org-mode
(defun miika/org-font-setup ()
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
    (set-face-attribute (car face) nil :font "Iosevka" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun miika/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . miika/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (miika/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  ;; :custom
  ;; (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  )


(defun miika/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(with-eval-after-load 'org
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t))))

;; (use-package visual-fill-column
;;   :hook (org-mode . miika/org-mode-visual-fill))

(miika/leader-keys
  :states '(normal visual)
  :keymap 'org-mode-map
  "e" '(:ignore t :which-key "Execute")
  "ed" '(org-babel-execute-src-block :which-key "Execute code block")
  "eb" '(org-babel-execute-buffer :which-key "Execute buffer")
  "me" '(org-edit-special :which-key "Edit Special"))

(with-eval-after-load 'org
  (org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

  (setq org-confirm-babel-evaluate nil))

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(setq org-cycle-emulate-tab nil)

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

;; Automatically tangle our Emacs.org config file when we save it
(defun miika/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/init.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'miika/org-babel-tangle-config)))

(use-package xkcd
  :commands (xkcd-get xkcd)
  :config
  (general-define-key
   :states '(normal emacs)
   :keymaps 'xkcd-mode-map
   "h" 'xkcd-prev
   "l" 'xkcd-next
   "r" 'xkcd-rand))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
