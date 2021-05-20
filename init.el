;; -*- lexical-binding: t -*-

(setq package-check-signature nil)
;; Initialize package sources
(require 'package)

;; package repositories
;; also set priority. Mainly to prefer Melpa Stable over Melpa.
(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa"        . "http://melpa.milkbox.net/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("org"          . "https://orgmode.org/elpa/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
	    ("gnu"          . 5)
	    ("melpa"        . 0)))

;; Hack for using a different set of repositories when ELPA is down
;; As soon as MELPA is back up again, comment out the section again.
;(setq package-archives
;      '(("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
;        ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
;        ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")))


;; list of packages to install.
;; Update: use use-package instead for consistency.
;; Only necessary to install use-package from here.
(setq package-selected-packages
      '(use-package))

;; execute all of your package autoloads (among other things)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; install packages from your user-installed packages list
(package-install-selected-packages)

;; set up use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; show line numbers in margin
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default indent-tabs-mode nil)

;; set default tab char's display width to 4 spaces
(setq-default tab-width 4)

;; indent level for languages
(setq js-indent-level 2)
(setq json-reformat:indent-width 2)

;; show cursor position within line
(column-number-mode 1)

;; sort results of apropos by relevancy
(setq apropos-sort-by-scores t)

;; add homebrew stuff to exec-path
(add-to-list 'exec-path "/usr/local/bin")

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; which-key brings up a completions menu after pressing a leader key 
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  ; delay time for which-key menu popup
  (setq which-key-idle-delay 0.15))

;; show paren matching
(show-paren-mode 1)

;; always insert left/right brackets together.
(electric-pair-mode 1)

;; make cursor movement stop in between camelCase words.
(global-subword-mode 1)

;; upon changing emacs versions, cmd-v no longer pastes in osx.
;; here we set cmd-v to paste from clipboard and cmd-c to copy to clipboard
(global-set-key (kbd "M-v") 'clipboard-yank)
(global-set-key (kbd "M-c") 'clipboard-kill-ring-save)


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; add to load path (this is where "require" looks for files, similar to PATH on unix)
(add-to-list 'load-path "~/.emacs.d/elisp-files")

;; load key-chord then define "fd" to enter normal mode
;load a file named key-chord.el from some directory in the load-path (e.g. "~/.emacs.d/elisp-files")
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "fd" 'evil-normal-state)

;; set font size (the value is in 1/10pt, so 100 will give you 10pt, etc.)
(set-face-attribute 'default nil :height 160)


(use-package dracula-theme
;  :init (load-theme 'dracula t)
  )

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; It is common for Emacs modes like Buffer Menu, Ediff, and others to define key bindings for RET and SPC. Since these are motion commands, Evil places its key bindings for these in evil-motion-state-map. However, these commands are fairly worthless to a seasoned Vim user, since they do the same thing as j and l commands. Thus it is useful to remove them from evil-motion-state-map so as when modes define them, RET and SPC bindings are available directly.
(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location. "
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

;; remove startup welcome screen
(setq inhibit-startup-message t)

(scroll-bar-mode -1)  ; disable visual scrollbar
(tool-bar-mode -1)    ; disable toolbar

;; Set up visual bell (flash the mode-line instead of default visual bell which is obnoxious)
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))


(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))


;; Swiper
(global-set-key (kbd "C-s") 'swiper-isearch)

;; ivy-rich adds doc strings to ivy/counsel buffers
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; helpful is an alternative to the built-in Emacs help that provides more contextual information and improved UI
(use-package helpful
  :custom
  ;; here we set it up to integrate with counsel
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ;; change describe-key keybindings to call helpful-key instead
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding for this in lisp modes.
  ("C-c C-d" . helpful-at-point))

;; crux adds some useful extensions that we will add to our leader-keys section later
(use-package crux)

(defun switch-buffer-scratch ()
  "Switch to the scratch buffer. If the buffer doesn't exist,
create it and write the initial message into it."
  (interactive)
  (let* ((scratch-buffer-name "*scratch*")
         (scratch-buffer (get-buffer scratch-buffer-name)))
    (unless scratch-buffer
      (setq scratch-buffer (get-buffer-create scratch-buffer-name))
      (with-current-buffer scratch-buffer
        (lisp-interaction-mode)
        (insert initial-scratch-message)))
    (switch-to-buffer scratch-buffer)))


;; General (leader-key bindings)
(use-package general
  :config
  (general-def :states '(normal motion) "SPC" nil) ; have to unbind space first before we can use it as a prefix key
  (general-create-definer ewh/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (ewh/leader-keys
    ; toggles
    "t"  '(:ignore t :which-key "toggles")
    "th" '(counsel-load-theme :which-key "choose theme")
    "tt" '(treemacs :which-key "Toggle tree view for files")

    ; files
    "f"  '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find file")
    "fz" '(counsel-fzf :which-key "fuzzy find file")
    "fi" '(crux-find-user-init-file :which-key "open emacs init file")
    "fn" '(crux-rename-file-and-buffer :which-key "Rename the current buffer and its visiting file if any")
    "ft" '(treemacs :which-key "Toggle tree view for files")
    "fr" '(recentf-open-files :which-key "Find recently opened files")

    ; buffers
    "b"  '(:ignore t :which-key "buffers")
    "bs" '(ivy-switch-buffer :which-key "switch buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "bk" '(kill-buffer :which-key "kill buffer")
    "bx" '(kill-current-buffer :which-key "kill current buffer")
    "ba" '(crux-kill-other-buffers :which-key "kill all other buffers")
    "bl" '(list-buffers :which-key "list buffers")
    "br" '(revert-buffer :which-key "revert buffer")
    "bc" '(switch-buffer-scratch :which-key "switch to scratch buffer")

    ; windows
    "w"  '(:ignore t :which-key "windows")
    "wj" '(evil-window-down :which-key "window down")
    "wk" '(evil-window-up :which-key "window up")
    "wh" '(evil-window-left :which-key "window left")
    "wl" '(evil-window-right :which-key "window right")
    "ws" '(evil-window-split :which-key "split window")
    "wv" '(evil-window-vsplit :which-key "vertical split window")
    "wx" '(evil-quit :which-key "close window")
    "wo" '(delete-other-windows :which-key "delete other windows")

    ; help
    "h"  '(:ignore t :which-key "help")
    "hk" '(helpful-key :which-key "describe key")
    "hf" '(counsel-describe-function :which-key "describe function")
    "hv" '(counsel-describe-variable :which-key "describe variable")
    "ha" '(counsel-apropos :which-key "apropos")
    "hp" '(helpful-at-point :which-key "describe symbol at point")
    "hm" '(describe-mode :which-key "describe mode")

    ; eval
    "e"  '(:ignore t :which-key "eval")
    "eb" '(eval-buffer :which-key "eval buffer")
    "er" '(eval-region :which-key "eval region")
    "ee" '(eval-expression :which-key "eval expression")
    "el" '(eval-last-sexp :which-key "eval last sexp before point")
    "ef" '(eval-defun :which-key "eval defun (top-level form containing point, or after point)")

    ; shell
    "s"  '(:ignore t :which-key "shell")
    "ss" '(shell :which-key "open shell")
    "se" '(eshell :which-key "open eshell")
    "st" '(term :which-key "open terminal emulator")

    ; misc
    "m"  '(:ignore t :which-key "misc")
    "mx" '(counsel-M-x :which-key "M-x")
    "mq" '(fill-paragraph :which-key "fill-paragraph")
    ))

;; set up PATH
;; Todo: move some of this setup inside use-package init?
(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (ewh/leader-keys
   "p" '(:keymap projectile-command-map :package projectile :which-key "projectile"))
  ;; NOTE: Set this to the folder where you keep your projects
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev")))
  ; load dired first thing upon switching projects
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)

(use-package org)

(use-package treemacs
  :ensure t)

(use-package treemacs-evil
  :ensure t)

; find recently opened files
(require 'recentf)
(recentf-mode 1)

(use-package evil-nerd-commenter
  ;; press alt-/ to comment/uncomment lines
  :bind ("M-/" . evilnc-comment-or-uncomment-lines)) 


; Language Server Protocol

(defun ewh/lsp-mode-setup ()
  ;; set up header breadcrumb which will show path info of the current file
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . ewh/lsp-mode-setup)
  :init
  ;; before setting up the leader key I have to setup the keymap-prefix or else which-key docs won't be available on the prefixes.
  (setq lsp-keymap-prefix "SPC l")
  (ewh/leader-keys
   "l" '(:keymap lsp-command-map :package lsp-mode :which-key "lsp"))
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package elm-mode
  :mode "\\.elm\\'"
  :hook ((elm-mode . lsp-deferred)
         (elm-mode . elm-format-on-save-mode))) ;format on save doesn't seem to work. Need to figure this out. For now calling elm-format manually works though.
  


(use-package json-mode)
(use-package js2-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-nerd-commenter doom-modeline doom-themes use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
