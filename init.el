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

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; Uncomment the following for debugging emacs startup
;; (setq use-package-verbose t)

(use-package auto-package-update
  :custom
  ;; (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))
  ;; (auto-package-update-at-time "09:00"))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defmacro with-system-not (type &rest body)
  "Evaluate BODY if `system-type' does not equal TYPE."
  (declare (indent defun))
  `(when (not (eq system-type ',type))
     ,@body))

(with-system darwin ;; Darqwin == MacOS
  (message "MacOS detected")
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'none
        miika/default-font "Monoid"
        miika/org-font "Monoid"
        miika/default-font-height 120))

(with-system gnu/linux
  (message "Linux detected")
  (setq miika/default-font "Monoid NF"
        miika/org-font "Monoid NF"
        miika/default-font-height 120))

(if (eq system-type 'windows-nt)
  (progn
    (message "Windows detected")
    (setq miika/init-file-path "c:/Users/mamoi/AppData/Roaming/.emacs.d/init.org"))
  (setq miika/init-file-path (expand-file-name "~/.emacs.d/init.org")))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(defun miika/open-user-init-file ()
  "Edit emacs config, in another window."
  (interactive)
  (find-file miika/init-file-path))


;; todo highlighting
(use-package hl-todo
:config (hl-todo-mode))


(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package command-log-mode
  :commands command-log-mode)

;; Hide native comp warnings
(setq native-comp-async-report-warnings-errors nil)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 5) ;; Padding on sides
(menu-bar-mode -1)
;; (setq visible-bell 1)
(setq ring-bell-function 'ignore)


(show-paren-mode 1)



(column-number-mode)
(global-display-line-numbers-mode t)
;; (setq display-line-numbers-type 'relative)


;; Disable line numbers from some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                jupyter-repl-mode-hook
                ))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(display-time-mode 1)

(set-face-attribute 'default nil :font miika/default-font :height miika/default-font-height :weight 'light)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-spacegrey t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1)
;;   :custom ((doom-modeline-height 15)))

(use-package nano-modeline
  :config
  (setq nano-modeline-position 'bottom)
  (nano-modeline-mode))

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

(use-package vertico
  :bind (:map vertico-map
              ("M-j" . vertico-next)
              ("M-k" . vertico-previous)
              ("C-f" . vertico-exit))
  :custom
  (vertico-cycle t)
  ;; :custom-face
  ;; (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :demand t
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(defun miika/consult-projectile-ripgrep (&optional initial)
    (interactive "P")
  (consult--grep "ripgrep" #'consult--grep-builder (projectile-project-root) initial))

(use-package embark
  :ensure t
  :bind
  (("M-." . embark-act)
   ("C-." . embark-dwin)
   ("C-h B" . embark-bindings)))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package rg
   :commands rg)

 (use-package ag
   :commands ag)

(use-package which-key
  :defer 0
  :diminish wich-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode))

;; (use-package ivy
;;   :diminish
;;   :bind (("C-s" . swiper)
;;          :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done)
;;          ("C-l" . ivy-alt-done)
;;          ("M-j" . ivy-next-line)
;;          ("M-k" . ivy-previous-line)
;;          :map ivy-switch-buffer-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-d" . ivy-reverse-i-search-kill))
;;   :config
;;   (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
;;   :init (ivy-mode 1))

;; (use-package ivy-xref
;;   :ensure t
;;   :init
;;   (when (>= emacs-major-version 27)
;;     (setq xref-show-definitions-function #'ivy-xref-show-defs))
;;   (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (use-package ivy-rich
;;   :after ivy
;;   :config (ivy-rich-mode 1))

;; (use-package counsel
;;   :after ivy
;;   :bind (("M-x" . counsel-M-x)
;;          ("C-x b" . counsel-ibuffer)
;;          ("C-x C-f" . counsel-find-file)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history))
;;   :config
;;   (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"))

;; (use-package ivy-prescient
;;   :after counsel
;;   :custom
;;   (ivy-prescient-enable-filtering nil)
;;   :config
;;   ;; Uncomment the following line to have sorting remembered across sessions!
;;   (prescient-persist-mode 1)
;;   (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  ;; :custom
  ;; (counsel-describe-function-function #'helpful-callable)
  ;; (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
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
  (evil-set-initial-state 'dashboard-mode 'normal)
  (setq evil-want-keybinding nil))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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
  :ensure t
  ;; :bind
  ;; (:map evil-multiedit-mode-map
  ;;       ("M-j" . evil-multiedit-next)
  ;;       ("M-k" . evil-multiedit-prev))
  :config
  (evil-multiedit-default-keybinds)
  (evil-multiedit-mode))

(use-package evil-easymotion)

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package general
  :config

  ;; (general-define-key
  ;;  :states 'insert
  ;;  :keymaps 'override
  ;;  "M-j" 'company-select-next
  ;;  "M-k" 'company-select-previous)

  (general-create-definer miika/leader-keys
    ;; :keymaps '(normal visual emacs)
    :states '(normal visual emacs)
    :prefix "SPC")

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'override
   "/" '(consult-line :which-key "Search")
   "n" '(newline :which-key "Inser newline")
   ;; "/" '(swiper :which-key "Search")
   )

  (miika/leader-keys
    ;; ":" '(counsel-M-x :which-key "M-x")
    ":" '(execute-extended-command :which-key "M-x")
    ";" '(eval-expression :which-key "Eval expression")
    "." '(consult-projectile :which-key "Consult projectile")
    ;; "." '(projectile-find-file :which-key "Find file in project")
    "SPC" '(:keymap evilem-map :which-key "Easy motion")
    "SPC s" '(evil-avy-goto-char
              :keymaps: 'override)
    "SPC S" '(evil-avy-goto-char-2
              :keymaps: 'override)

    "s"  '(:ignore t :which-key "Search")
    "ss" '(consult-ripgrep :which-key "Ripgrep cur dir")
    "sp" '(miika/consult-projectile-ripgrep :which-key "Ripgrep project")

    "x" '(:keymap ctl-x-map :which-key "C-x")
    "c" '(:keymap mode-specific-map :which-key "C-c")
    "h" '(:keymap help-map :which-key "Help")

    ;; Buffers
    "b" '(:ignore t :which-key "Buffer")
    "bb" '(consult-buffer :which-key "Switch to buffer")
    "bv" '(miika/switch-to-vterm-buffer :which-key "Switch to vterm buffer")
    "bk" '(kill-current-buffer :which-key "Kill current buffer")
    "bl" '(evil-switch-to-windows-last-buffer :which-key "Next buffer")

    ;; Toggle
    "t" '(:ignore t :which-key "Toggle")
    "tt" '(consult-theme :which-key "Load theme")
    "ts" '(hydra-text-scale/body :which-key "Scale text")
    "te" '(treemacs :which-key "Toggle treemacs")


    ;; Window management
    "w" '(:keymap evil-window-map :package evil)
    "ww" '(miika/focus-next-window-or-open-new
           :keymaps 'override
           :which-key "Focus on next window or open new")

    ;; Files
    "f" '(:ignore t :which-key "File")
    "fi" '(miika/open-user-init-file :which-key "Open init.el")
    "ff" '(find-file :which-key "Find file")
    ;; "ff" '(find-file :which-key "Find file")
    ;; "f ." '(projectile-find-file-in-directory :which-key "Find file in dir")

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

(use-package format-all
  :commands (format-all-buffer format-all-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  ;; :custom ((projectile-completion-system 'ivy))
  ;; :bind-keymap
  ;; ("SPC p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (setq projectile-project-search-path '("~/dev" "~/learning"))
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (setq projectile-globally-ignored-directories
        (append '(".bloop" ".bsp" ".metals" "target" ".mypy_cache")
                projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-files (append '(".#*" "#*") projectile-globally-ignored-files))
  (setq projectile-enable-caching nil))

;; (use-package counsel-projectile
;;   :config (counsel-projectile-mode))

(use-package consult-projectile)

(use-package treemacs
  :commands treemacs)

(use-package treemacs-projectile
  :after treemacs)

(use-package treemacs-magit
  :after treemacs)

(use-package flycheck
  :defer t
  :config
  (global-flycheck-mode)
  (miika/leader-keys
    :keymap flycheck-mode-map
    "ne" '(flycheck-next-error :which-key "Go to next error")))

(defun miika/company-complete-selection ()
  "Insert the selected candidate or the first if none are selected.
    From: https://www.reddit.com/r/emacs/comments/kmeuft/companymode_not_autocompleting_first_candidate/"
  (interactive)
  (if company-selection
      (company-complete-selection)
    (company-complete-number 1)))

(use-package company
  ;; :after (lsp-mode emacs-lisp-mode)
  :hook ((emacs-lisp-mode . company-mode)
         (lsp-mode . company-mode))
  :bind
  (:map company-active-map
        ("<tab>" . miika/company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.1)
  :config
  (global-company-mode))

;; Nicer UI
(use-package company-box
  :hook (company-mode . company-box-mode))

;; (use-package corfu
;;   :ensure t
;;   :bind
;;   (:map corfu-map
;;         ("M-j" . corfu-next)
;;         ("M-k" . corfu-previous)
;;         ("<tab>" . corfu-insert))
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-preview-current t)
;;   :config
;;   (setq tab-always-indent 'complete)
;;   (corfu-global-mode))

(use-package eglot
  :ensure t
  :config
  ;; (eglot-work)
  (setq eglot-stay-out-of '(flymake))
  (miika/leader-keys
    :keymap eglot-mode-map
    "r" '(:ignore t :which-key "Refactor")
    "rr" '(eglot-rename :which-key "Rename symbol")))

(use-package smartparens
  :after evil
  :config
  (smartparens-global-mode t)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
  (sp-pair "'" nil :actions :rem))

(use-package evil-smartparens
  :after smartparens
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package rainbow-delimiters)

(use-package hl-todo
  :ensure t
  :config
  (setq global-hl-todo-mode t))

(use-package paredit
    :config
    (miika/leader-keys
      :keymaps 'paredit-mode-map
      :states '(normal visual)
      "kd" '(paredit-forward-barf-sexp :which-key "Forward barf sexp")
      "kD" '(paredit-backward-barf-sexp :which-key "Backward barf sexp")
      "ks" '(paredit-forward-slurp-sexp :which-key "Forward slurp sexp")
      "kS" '(paredit-backward-slurp-sexp :which-key "Backward slurp sexp")))

      (use-package aggressive-indent)

(miika/leader-keys
  :keymaps 'emacs-lisp-mode-map
  :states '(normal visual)
  ;; Eval
  "e" '(:ignore t :which-key "Eval")
  "ed" '(eval-defun :which-key "Eval defun")
  "er" '(eval-region :which-key "Eval region")
  "eb" '(eval-region :which-key "Eval buffer"))

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

(use-package cider
  :hook clojure-mode
  :config
  (miika/leader-keys
    :keymap 'clojure-mode-map
    "ms" '(:ignore t :which-key "Cider")
    "msi" '(cider-jack-in :which-key "Cider jack-in")
    "msj" '(cider-jack-in-cljs :which-key "Cider jack-in cljs")
    "e" '(:ignore t :which-key "Eval")
    "er" '(cider-eval-region :which-key "Eval region")
    "ed" '(cider-eval-defun-at-point :which-key "Eval defun")
    "eb" '(cider-eval-buffer :which-key "Eval buffer")
    "mf" '(:ignore t :which-key "Format")
    "mfa" '(cider-format-buffer :which-key "Format buffer")
    "mfr" '(cider-format-region :which-key "Format region")
    "mfd" '(cider-format-defun :which-key "Format defun")))

(use-package scala-mode
  :mode "\\.scala\\'"
  :interpreter
  ("scala" . scala-mode)
  :hook ((scala-mode . eglot-ensure))
  :config
  (miika/leader-keys
    :keymap scala-mode-map
    "mfa" '(eglot-format-buffer :which-key "Format buffer")
    "mfr" '(eglot-format :which-key "Format Region")))



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

;; (use-package lsp-metals
;;   :after scala-mode
;;   :config
;;   (setq lsp-metals-treeview-show-when-views-received nil))

(defun miika/open-ipython-repl ()
  "Open an IPython REPL."
  (interactive)
  (require 'python)
  (let ((python-shell-interpreter "ipython")
        (python-shell-interpreter-args "-i --simple-prompt --no-color-info"))
    (pop-to-buffer
     (process-buffer (run-python nil nil t)))))

(defun miika/open-python-repl ()
  "Open a normal python REPL."
  (interactive)
  (require 'python)
  (let ((python-shell-interpreter "python")
        (python-shell-interpreter-args "-i --simple-prompt --no-color-info"))
    (pop-to-buffer
     (process-buffer (run-python nil nil t)))))

(setq python-shell-interpreter (expand-file-name "~/miniconda3/bin/python"))

(use-package python-black
  :demand t
  :after python)

(defun miika/python-setup ()
  "Setup Python"
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-offset 4)
  (setq python-shell-completion-native-enable nil) ; IPython repl breaks without this ATM
  ;; (setq lsp-completion-mode t)
  ;; (flymake-mode-off)
  (miika/leader-keys
    :keymap 'python-mode-map
    "mw" '(conda-env-activate :which-key "Workon enviroment")
    ;; "mw" '(pyvenv-workon :which-key "Workon enviroment")
    "ms" '(:ignore t :which-key "Shell")
    "mss" '(run-python :which-key"Python shell")
    ;; "msi" '(miika/open-ipython-repl :which-key "Ipython shell")
    "msi" '(miika/open-python-repl :which-key "Python shell")
    "msj" '(miika/open-jupyter-repl :which-key "Jupyter shell")
    "msr" '(python-shell-send-region :which-key "Send region")
    "msd" '(python-shell-send-defun :which-key "Send defun")
    "msb" '(python-shell-send-buffer :which-key "Send buffer")
    "msf" '(python-shell-send-file :which-key "Send file")
    "mfa" '(python-black-buffer :which-key "Format buffer")
    "mfr" '(python-black-format-region :which-ley "Format region"))
  (message "Python mode activated"))

(add-hook 'python-mode-hook 'miika/python-setup)
;; (add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'miika/conda-autoactivate)

(defun miika/conda-env-activate (name)
  "Switch to environment NAME."
  (let* ((env-name name)
         (env-dir (conda-env-name-to-dir env-name)))
    (conda-env-activate-path env-dir)))

(defun miika/conda-autoactivate ()
  "Sets up conda environment based on project directory."
  (message "Python mode detected. Trying auto conda env activation.")
  (let ((project-name (projectile-project-name))
        (envs (conda-env-candidates)))
    (message (concat "Activating conda environment " project-name))
    (if (member project-name envs)
        (progn
          (conda-env-activate project-name)
          (message (concat "Conda env " project-name " activated")))
      (message (concat "No such environment as " project-name)))))


(defun miika/python-after-env-activate-setup ()
  "Sets up python after evirnoment activation"
  (setq python-shell-interpreter (expand-file-name "bin/python" conda-env-current-path))
  (eglot-ensure))


(use-package conda
  :commands (conda-env-activate
             conda-env-list
             conda-env-candidates)
  :config
  (custom-set-variables
   '(conda-anaconda-home (expand-file-name "~/miniconda3/")))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3/"))
  (conda-env-initialize-interactive-shells)
  ;; (conda-env-autoactivate-mode t)
  (add-to-list 'global-mode-string
               '(conda-env-current-name (" conda:" conda-env-current-name " "))
               'append)
  (conda-env-initialize-eshell)
  ;; Make sure lsp is started/restarted after conda env is initialized
  (add-hook 'conda-postactivate-hook #'miika/python-after-env-activate-setup))

(setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs"))

;; (use-package pyvenv
;;   ;; :diminish
;;   :config
;;   (setq pyvenv-mode-line-indicator
;;         '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
;;     (add-hook 'pyvenv-post-activate-hooks #'miika/python-after-env-activate-setup)
;;   (pyvenv-mode +1))

(defun miika/jupyter-run-repl (kernel-name &optional repl-name associate-buffer client-class display)
  "Same as jupyter-run-repl but non interactive call finds kernelspecs with display name instead of kernel name."
  (interactive (list (car (jupyter-completing-read-kernelspec
                           nil current-prefix-arg))
                     (when current-prefix-arg
                       (read-string "REPL Name: "))
                     t nil t))
  (or client-class (setq client-class 'jupyter-repl-client))
  (jupyter-error-if-not-client-class-p client-class 'jupyter-repl-client)
  (unless (called-interactively-p 'interactive)
    (or (when-let* ((name (car (miika/jupyter-find-kernelspecs-by-display-name kernel-name))))
          (setq kernel-name name))
        (error "No kernel found for prefix (%s), run python -m ipykernel install --user --name=$CONDA_DEFAULT_ENV to install kernell from conda env" kernel-name)))
  ;; For `jupyter-start-new-kernel', we don't require this at top-level since
  ;; there are many ways to interact with a kernel, e.g. through a notebook
  ;; server, and we don't want to load any unnecessary files.
  (require 'jupyter-kernel-process-manager)
  (cl-destructuring-bind (_manager client)
      (jupyter-start-new-kernel kernel-name client-class)
    (jupyter-bootstrap-repl client repl-name associate-buffer display)))

(defun miika/jupyter-find-kernelspecs-by-display-name (name &optional refresh)
  "Find jupyter kernel specs by display name"
  (let* ((specs (jupyter-available-kernelspecs refresh))
         (display-names (if (null specs) (error "No kernelspecs available")
                          (mapcar (lambda (k) (plist-get (cddr k) :display_name))
                             specs))))
    (nth (- (length display-names)
            (length (member name display-names)))
         specs)))

(defun miika/open-jupyter-repl ()
  "Open a Jupyter REPL:"
  (interactive)
  (miika/jupyter-run-repl conda-env-current-name))

(use-package jupyter
  :commands (miika/open-jupyter-repl
             miika/run-jupyter-repl
             jupyter-run-server-repl
             jupyter-run-repl
             jupyter-server-list-kernels))

;; (use-package web-mode
;;   :mode ("\\.tsx\\'" "\\.jsx\\'")
;;   :ensure t)

;; (use-package typescript-mode
;;   :ensure t)

;; (use-package tide
;;   :ensure t
;;   :config
;;   (miika/leader-keys
;;     :keymap tide-mode-map
;;     "r" '(:ignore t :which-key "Refactor")
;;     "rr" '(tide-rename-symbol-at-location :which-key "Rename symbol")
;;     "rf" '(tide-rename-file :which-key "Rename file")
;;     "gd" '(tide-jump-to-definition :which-key "Jump to definition")
;;     "gr" '(tide-references :which-key "Goto reference")))

;; (defun miika/setup-tide-mode ()
;;   (interactive)
;;   (flycheck-mode +1)
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))

;; (add-hook 'typescript-mode-hook #'miika/setup-tide-mode)



;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (miika/setup-tide-mode))))

;; (flycheck-add-mode 'typescript-tslint 'web-mode)

(setq-default c-basic-offset 4)

(use-package clang-format
  :commands (clang-format-buffer clang-format-region))

(use-package cc-mode
  :config
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)
  (with-eval-after-load 'lsp-mode
    (require 'dap-cpptools))
  (miika/leader-keys
    :keymaps '(c++-mode-map c-mode-map)
    "mc" '(compile :which-key "Compile file")
    "mfa" '(clang-format-buffer :which-key "Format buffer")
    "mfr" '(clang-format-region :which-key "Format region")))

(use-package csharp-mode
    :mode "\\.cs\\'"
    :config
    (add-hook 'csharp-mode-hook 'eglot-ensure))

;; https://github.com/joaotavora/eglot/issues/241

(use-package shader-mode
  :mode "\\.shader\\'"
  :mode "\\.compute\\'")

;; https://github.com/godotengine/emacs-gdscript-mode#known-issues
(defun lsp--gdscript-ignore-errors (original-function &rest args)
  "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
  (if (string-equal major-mode "gdscript-mode")
      (let ((json-data (nth 0 args)))
        (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                 (not (gethash "id" json-data nil))
                 (not (gethash "method" json-data nil)))
            nil ; (message "Method not found")
          (apply original-function args)))
    (apply original-function args)))
;; Runs the function `lsp--gdscript-ignore-errors` around `lsp--get-message-type` to suppress unknown notification errors.

(use-package gdscript-mode
  :mode "\\.gd\\'"
  :config
  (add-hook 'gdscript-mode-hook 'lsp-deferred)
  (advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)
  (setq gdscript-godot-executable (expand-file-name "~/bin/godot")))

(use-package markdown-preview-mode
  :after (markdown-mode))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package docker-compose-mode)

(use-package highlight-indent-guides
  :ensure t
  :commands highlight-indent-guides-mode)

(use-package yaml-mode
  :hook (format-all highlight-indent-guides-mode)
  :config
  (miika/leader-keys
    :states '(normal visual)
    :keymap 'org-mode-map
    "mf" '(:ignore t :which-key "Format")
    "mfa" '(format-all-buffer :which-key "Format buffer")))

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

;; vterm doesn't work on windows sadly
(with-system-not 'windows-nt

  (use-package multi-vterm
    :ensure t)

  (use-package vterm
    :after (multi-vterm)
    :commands (vterm vterm-other-window vterm-mode)
    :config
    (with-eval-after-load 'em-term
      (defun eshell-exec-visual (&rest args)
        (apply #'eshell-exec-in-vterm args)))
    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
    (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
    (setq vterm-max-scrollback 10000)))

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
    (set-face-attribute (car face) nil :font miika/org-font :weight 'regular :height (cdr face)))

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
  (miika/org-font-setup)
  )

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(defvar miika/bibs '("~/bib/references.bib"))

(use-package citeproc
  :config
  (setq org-cite-csl-styles-dir "~/bib/csl")
  (setq org-cite-csl--fallback-style-file "~/bib/csl/apa.csl")
  (setq org-cite-export-processors '((t csl))))

(use-package citar
  :after org
  ;; :commands org-cite-insert
  :custom (citar-bibliography miika/bibs)
  :config
  (miika/leader-keys
    :keymap 'org-mode-map
    "ci" '(org-cite-insert :which-key "Insert citation"))
  (miika/leader-keys
    :keymap 'LaTeX-mode-map
    "ci" '(citar-insert-citation :which-key "Insert citation"))
  (setq org-cite-global-bibliography miika/bibs)
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar))

(defun miika/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t))))

;; Citation styles: https://blog.tecosaur.com/tmio/2021-07-31-citations.html#cite-styles

;; (use-package visual-fill-column
;;   :hook (org-mode . miika/org-mode-visual-fill))

(miika/leader-keys
  :states '(normal visual)
  :keymap 'org-mode-map
  "e" '(:ignore t :which-key "Execute/Export")
  "ed" '(org-babel-execute-src-block :which-key "Execute code block")
  "eb" '(org-babel-execute-buffer :which-key "Execute buffer")
  "ee" '(org-export-dispatch :which-key "Export")
  "me" '(org-edit-special :which-key "Edit Special"))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)))

  (setq org-confirm-babel-evaluate nil))

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("js" . "src javascript")))

(setq org-cycle-emulate-tab nil)

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

;; Automatically tangle our Emacs.org config file when we save it
(defun miika/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      miika/init-file-path)
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'miika/org-babel-tangle-config)))

;; (add-hook 'latex-mode-hook 'company-mode)

(use-package auctex
  :after 'LaTeX-mode)

(use-package flycheck-grammarly
  :hook latex-mode)

(use-package emojify
  :hook (after-init . global-emojify-mode))

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

(use-package gcmh
  :init (gcmh-mode 1))
