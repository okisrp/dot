;;; init.el --- Main configuration. -*- lexical-binding: t -*-

;; Stop displaying startup screen.
(setq inhibit-startup-message t
      inhibit-splash-screen t
      inhibit-startup-screen t)

;; Stop providing information in startup echo area message.
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))

;; Delete comments from scratch buffer.
(setq initial-scratch-message nil)

;; Set simple text alike mode in scratch buffer instead of
;; `lisp-interaction-mode'.
(setq initial-major-mode 'fundamental-mode)

;; Stop asking whether to follow symlinks or not.
(setq vc-follow-symlinks nil)

;; Get the latest version of `straight.el', but it's going to be less
;; stable.
(setq straight-repository-branch "develop")

;; Configure an alternative way for `straight.el' to check for
;; modifications made to package source code in order to speed up
;; starup time.
(setq straight-check-for-modifications nil)

;; Next-generation, purely functional package manager.
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

;; Reduce the size of repositories.
(setq straight-vc-git-default-clone-depth 1)

;; If you use `use-package', then this makes each 'use-package' form
;; also invoke `straight.el' to install the packages, unless otherwise
;; specified.
(setq straight-use-package-by-default t)

;; To use `use-package', first install it with `straight.el'.
(straight-use-package 'use-package)

;; The community-driven pastel theme that aims to be the middle ground
;; between low and high contrast themes.
(use-package catppuccin-theme
  :straight
  (catppuccin-theme :host github :repo "catppuccin/emacs")
  :custom
  (catppuccin-flavor 'latte)
  (catppuccin-highlight-matches t)
  (catppuccin-italic-comments nil)
  :config
  ;; Change `catppuccin-theme' when inside of a terminal.
  (unless (window-system)
    (setq catppuccin-flavor 'mocha))
  (load-theme 'catppuccin 'no-confirm)
  (defface catppuccin-theme-visible-bell
    `((t :background ,(catppuccin-get-color 'maroon)
		 :foreground ,(catppuccin-get-color 'base)))
    "Define face for `catppuccin-theme' visible bell implementation.")
  (defun catppuccin-theme-visible-bell-fn (&rest _)
    "Implement the logic for visible bell function. "
    (let* ((face (if (facep 'mode-line-active)
                     'mode-line-active
                   'mode-line))
           (buf (current-buffer))
           (cookie (face-remap-add-relative face 'catppuccin-theme-visible-bell)))
      (force-mode-line-update)
      (run-with-timer 0.15 nil
                      (lambda ()
						(with-current-buffer buf
                          (face-remap-remove-relative cookie)
                          (force-mode-line-update))))))
  ;; Enable visible bell.
  (setq ring-bell-function 'catppuccin-theme-visible-bell-fn
		visible-bell t)
  ;; Kill annoying `catppuccin-definitions' buffer.
  (when-let ((buf (get-buffer "catppuccin-definitions.el")))
	(kill-buffer buf))
  ;; Highlight line at point.
  (global-hl-line-mode 1)
  ;; Disable cursor blinking.
  (blink-cursor-mode 0))

;; Customize mode line (I don't know what is going on here).
(setq-default mode-line-format
			  '("%e"
				(:propertize " " display (raise +0.1))
				(:propertize " " display (raise -0.1))
				mode-line-frame-identification
				mode-line-buffer-identification
				(:eval (propertize
						" " 'display
						`((space :align-to
								 (-  (+ right right-fringe right-margin)
									 ,(+ 2 (string-width "%4l:3%c")))))))
				(:propertize "%4l:%c" face mode-line-buffer-id)))

;; Set default font family for `default', that's actually more than
;; enough, and `fixed-pitch' faces.
(let ((font-family "Victor Mono"))
  (when (member font-family (font-family-list))
    (dolist (face '(default fixed-pitch))
      (set-face-attribute face nil :font (font-spec :family font-family :size 20 :weight 'medium)))))

;; Get access to recent visited files.
(recentf-mode 1)

;; Save minibuffer history.
(savehist-mode 1)

;; Save cursor position of each file.
(save-place-mode 1)

;; Update a buffer when it has been changed.
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Don't pile up a heap of `dired' buffers.
(setq dired-kill-when-opening-new-dired-buffer t)

;; Use shorter answers `y-or-n-p'.
(setq use-short-answers t)

;; Delete file to trash bin.
(when-let ((dir (getenv "XDG_DATA_HOME")))
  (setq trash-directory (expand-file-name "Trash/files/" dir)
		delete-by-moving-to-trash t))

;; Ignore case whether it's lower or upper one.
(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

;; Scroll by one line without twitching.
(setq scroll-conservatively (expt 2 7))

;; Make scrolling normal.
(setq mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

;; Trim extra lines and spaces when saving a file.
(add-hook 'write-file-functions 'delete-trailing-whitespace)

;; Delete tab instead of space when it's a tab.
(setq backward-delete-char-untabify-method 'hungry)

;; Delete selected text when start to type.
(delete-selection-mode 1)

;; Enable tabs instead of space and set their size.
(setq-default tab-width 4
			  indent-tabs-mode t)

;; Enable auto-pairing in programming and configuration modes.
(dolist (hook '(prog-mode-hook conf-mode-hook))
  (add-hook hook 'electric-pair-local-mode))

;; Construct unique buffer names for files with the same base name.
(setq uniquify-buffer-name-style 'forward)

;; Ignore buffers that start with asterisk when switching between them.
(setq switch-to-prev-buffer-skip-regexp "\*[^*]+\*")

;; Specify how should buffers behave.
(setq display-buffer-alist
      '(("\\*[Hh]elp*" (display-buffer-same-window))
		("\\*Occur\\*" (display-buffer-same-window))))

;; Open `Man' buffers in the same window.
(setq Man-notify-method 'pushy)

;; Replace `help' buffer for `helpful'.
(use-package helpful
  :bind
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ("C-h c" . helpful-callable)
  ("C-h d" . helpful-at-point))

;; Tell which key should be pressed.
(use-package which-key
  :custom
  (which-key-min-column-description-width 20)
  :config
  (which-key-mode 1))

;; Make vertical completion.
(use-package vertico
  :bind (:map vertico-map
			  ("M-p" . vertico-previous)
			  ("M-n" . vertico-next))
  :custom
  (vertico-scroll-margin 2)
  (vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle t)
  :init
  (vertico-mode 1))

;; Make no order.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Add description for vertical completion.
(use-package marginalia
  :init
  (marginalia-mode 1))

;; Set second input method.
(setq default-input-method "ukrainian-computer")

;; NOTE: `google-translate' package somehow messes up with
;; `default-input-method' variable or something like that.
(defun toggle-im (&rest _)
  (interactive)
  (if current-input-method
	  (deactivate-input-method)
	(set-input-method (or default-input-method "ukrainian-computer"))))

;; Invoke `toggle-im' instead of `toggle-input-method'.
(bind-key [remap toggle-input-method] 'toggle-im)

;; Use translator when learning another language.
(use-package google-translate
  :init
  (setq google-translate-preferable-input-methods-alist
        '((nil) ("ukrainian-computer" . ("uk" "ru"))))
  :config
  ;; Make `google-translate' listen button normal sized.
  (set-face-attribute
   'google-translate-listen-button-face nil :height 1.0)
  (setq google-translate-listen-button-label "[Listen]"
        google-translate-listen-program (executable-find "mplayer")
        google-translate-default-source-language "en"
        google-translate-default-target-language "uk"
        google-translate-pop-up-buffer-set-focus nil
        google-translate-output-destination 'help
        google-translate-backend-method 'curl
        google-translate-show-phonetic t
        google-translate-display-translation-phonetic nil
        google-translate-input-method-auto-toggling t)
  (defun google-translate-clipboard (&rest _)
	"Translate text from clipboard using `google-translate' package."
    (interactive)
    (let ((source google-translate-default-source-language)
          (target google-translate-default-target-language)
          (output google-translate-output-destination)
          (text (gui-get-selection 'CLIPBOARD 'TEXT)))
      (google-translate-translate source target text output)))
  ;; Make leader key for `google-translate' functions.
  (bind-keys :prefix "M-o"
			 :prefix-map M-o-prefix-map
			 ("q" . google-translate-query-translate)
			 ("r" . google-translate-query-translate-reverse)
			 ("s" . google-translate-smooth-translate)
			 ("w" . google-translate-at-point)
			 ("c" . google-translate-clipboard)))

;; Make ESC key behave as it should.
(bind-key "<escape>" 'keyboard-escape-quit)

;; Don't want to hit these keys by accident.
(dolist (key '("C-x C-u" "C-z"))
  (unbind-key key))

;; Make more intuitive `undo' and `undo-redo' keybindings.
(bind-keys ("C-z" . undo)
		   ("C-S-z" . undo-redo))

;; Make moving around buffer more faster.
(bind-keys ("M-p" . previous-buffer)
		   ("M-n" . next-buffer))

;; Make something similar to leader key.
(bind-keys :prefix "M-SPC"
		   :prefix-map M-SPC-prefix-map
		   ("k" . kill-current-buffer)
		   ("i" . ibuffer)
		   ("r" . recentf-open-files))

(use-package consult
  :bind
  ("M-s l" . consult-line)
  ("M-s r" . consult-ripgrep)
  ("M-s m" . consult-line-multi)
  ("M-s f" . consult-find))

;; Do not use user interface to configure, but if you do then:
(setq custom-file (expand-file-name "custom.el" old-user-emacs-dir))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))
