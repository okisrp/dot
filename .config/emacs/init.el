;;; init.el --- Main configuration. -*- lexical-binding: t -*-

(setq inhibit-startup-screen t)

(defun display-startup-echo-area-message (&rest _)
  (let ((time (format "%.1f" (float-time (time-subtract after-init-time before-init-time)))))
    (message "Loaded in %s seconds with %d garbage collection." time gcs-done)))

(setq idle-update-delay 1.0)

(setq straight-repository-branch "develop"
      straight-base-dir (expand-file-name "tmp/" user-emacs-directory)
	  straight-check-for-modifications nil)

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

(setq straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t)

(straight-use-package 'use-package)

(use-package catppuccin-theme
  :straight
  (catppuccin-theme :host github :repo "catppuccin/emacs")
  :custom
  (catppuccin-flavor 'latte)
  (catppuccin-highlight-matches t)
  (catppuccin-italic-comments t)
  :config
  (unless (window-system)
	(setq catppuccin-flavor 'mocha))
  (load-theme 'catppuccin 'no-confirm)
  (defface catppuccin-theme-visible-bell
    `((t :background ,(catppuccin-get-color 'maroon)
		 :foreground ,(catppuccin-get-color 'base))) "")
  (defun catppuccin-theme-visible-bell-fn (&rest _)
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
  (setq ring-bell-function 'catppuccin-theme-visible-bell-fn
		visible-bell t)
  (global-hl-line-mode 1)
  (blink-cursor-mode 0))

(setq-default mode-line-format
			  '("%e"
				(:propertize " " display (raise +0.1))
				(:propertize " " display (raise -0.1))
				(:propertize "λ " face font-lock-comment-face)
				mode-line-frame-identification
				mode-line-buffer-identification
				(:eval (propertize
						" " 'display
						`((space :align-to
								 (-  (+ right right-fringe right-margin)
									 ,(+ 2 (string-width "%4l:3%c")))))))
				(:propertize "%4l:%c" face mode-line-buffer-id)))

(let ((font-family "Victor Mono"))
  (when (member font-family (font-family-list))
    (dolist (face '(default fixed-pitch))
      (set-face-attribute face nil :font (font-spec :family font-family :size 20 :weight 'medium)))))

(setq-default tab-width 4
			  indent-tabs-mode t)

(add-hook 'write-file-functions 'delete-trailing-whitespace)

(dolist (hook '(prog-mode-hook conf-mode-hook))
  (add-hook hook 'electric-pair-local-mode))

(setq uniquify-buffer-name-style 'forward)

(setq switch-to-prev-buffer-skip-regexp "\*[^*]+\*")

(setq display-buffer-alist
      '(("\\*[Hh]elp*" (display-buffer-same-window))
		("\\*Occur\\*" (display-buffer-same-window))
		("\\*Shell Command Output\\*" (display-buffer-same-window))))

(setq Man-notify-method 'pushy)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory)))
      make-backup-file t)

(let ((directory (expand-file-name "tmp/lock-files/" user-emacs-directory)))
  (unless (file-directory-p directory)
    (make-directory directory 'parents))
  (setq lock-file-name-transforms `((".*" ,directory t))
		create-lockfiles t))

(let ((directory (expand-file-name "tmp/auto-saves/" user-emacs-directory)))
  (unless (file-directory-p directory)
    (make-directory directory 'parents))
  (setq auto-save-list-file-prefix (expand-file-name "sessions/" directory)
		auto-save-file-name-transforms `((".*" ,directory t))))

(setq recentf-save-file (expand-file-name "tmp/recentf.el" user-emacs-directory)
	  recentf-exclude '("emacs/tmp/")
	  recentf-max-saved-items 20)
(recentf-mode 1)

(setq savehist-file (expand-file-name "tmp/history.el" user-emacs-directory)
      history-length 20)
(savehist-mode 1)

(setq save-place-file (expand-file-name "tmp/places.el" user-emacs-directory))
(save-place-mode 1)

(setq transient-history-file (expand-file-name "tmp/transient/history.el" user-emacs-directory)
      transient-levels-file (expand-file-name "tmp/transient/levels.el" user-emacs-directory)
      transient-values-file (expand-file-name "tmp/transient/values.el" user-emacs-directory))

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(when-let ((directory (or (getenv "XDG_DATA_HOME")
						  (expand-file-name ".local/share/" (getenv "HOME"))))
		   (_ (file-directory-p directory)))
  (setq trash-directory (expand-file-name "Trash/files" directory)
		delete-by-moving-to-trash t))

(setq bookmark-default-file (expand-file-name "tmp/bookmarks.el" user-emacs-directory)
	  bookmark-save-flag t)

(setq dired-kill-when-opening-new-dired-buffer t)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(setq backward-delete-char-untabify-method 'hungry)

(delete-selection-mode 1)

(setq scroll-step 1
      scroll-margin 2)

(setq mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(setq use-short-answers t)

(bind-key "<escape>" 'keyboard-escape-quit)

(dolist (key '("C-x C-u" "C-z"))
  (unbind-key key))

(bind-keys ("C-z" . undo)
		   ("C-S-z" . undo-redo))

(bind-keys ("M-p" . previous-buffer)
		   ("M-n" . next-buffer))

(bind-keys :prefix "M-SPC"
		   :prefix-map M-SPC-prefix-map
		   ("k" . kill-current-buffer)
		   ("i" . ibuffer)
		   ("r" . recentf-open-files))

(use-package helpful
  :bind
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  ("C-h c" . helpful-callable)
  ("C-h d" . helpful-at-point))

(use-package which-key
  :custom
  (which-key-min-column-description-width 20)
  :config
  (which-key-mode 1))

(setq completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t)

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

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(setq default-input-method "ukrainian-computer")

;; NOTE: `google-translate' package somehow messes up with
;; `default-input-method' variable or something like that.
(defun toggle-im (&rest _)
  (interactive)
  (if current-input-method
	  (deactivate-input-method)
	(set-input-method (or default-input-method "ukrainian-computer"))))

(bind-key [remap toggle-input-method] 'toggle-im)

(use-package google-translate
  :init
  (setq google-translate-preferable-input-methods-alist
        '((nil) ("ukrainian-computer" . ("uk" "ru"))))
  :config
  (set-face-attribute
   'google-translate-listen-button-face nil :height 1.0)
  (setq google-translate-listen-button-label "[Listen]"
        google-translate-listen-program (executable-find "mplayer")
        google-translate-default-source-language "en"
        google-translate-default-target-language "uk"
        google-translate-pop-up-buffer-set-focus t
        google-translate-output-destination 'help
        google-translate-backend-method 'curl
        google-translate-show-phonetic t
        google-translate-display-translation-phonetic nil
        google-translate-input-method-auto-toggling t)
  (defun google-translate-clipboard (&rest _)
    (interactive)
    (let ((source google-translate-default-source-language)
          (target google-translate-default-target-language)
          (output google-translate-output-destination)
          (text (gui-get-selection 'CLIPBOARD 'TEXT)))
      (google-translate-translate source target text output)))
  (bind-keys :prefix "M-o"
			 :prefix-map M-o-prefix-map
			 ("q" . google-translate-query-translate)
			 ("r" . google-translate-query-translate-reverse)
			 ("s" . google-translate-smooth-translate)
			 ("w" . google-translate-at-point)
			 ("c" . google-translate-clipboard)))
