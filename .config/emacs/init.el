;;; init.el --- Initial configuration. -*- lexical-binding: t -*-

(setq inhibit-startup-screen t)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(unless (file-directory-p (expand-file-name "tmp/auto-saves/" user-emacs-directory))
  (make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) 'parent))

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(unless (file-directory-p (expand-file-name "tmp/lock-files/" user-emacs-directory))
  (make-directory (expand-file-name "tmp/lock-files/" user-emacs-directory) 'parent))

(setq lock-file-name-transforms `((".*" ,(expand-file-name "tmp/lock-files/" user-emacs-directory) t)))

(setq custom-file (locate-user-emacs-file "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(setq initial-buffer-choice (locate-user-emacs-file "init.el"))

(let ((font-family "Iosevka Term"))
  (when (member font-family (font-family-list))
    (dolist (face '(default fixed-pitch))
      (set-face-attribute face nil :font (font-spec :family font-family :size 20)))))

(setq help-window-select t)

(add-to-list 'display-buffer-alist '("*Help*" display-buffer-same-window))

(setq scroll-step 1
      scroll-margin 9)

(bind-key "<escape>" 'keyboard-escape-quit)

(bind-keys ("M-p" . previous-buffer)
	   ("M-n" . next-buffer))

(setq straight-repository-branch "develop")

(setq straight-base-dir (expand-file-name "tmp/" user-emacs-directory))

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

(setq straight-vc-git-default-clone-depth 1)

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(use-package catppuccin-theme
  :straight (catppuccin-theme :host github :repo "catppuccin/emacs")
  :custom
  (catppuccin-flavor 'latte)
  :config
  (load-theme 'catppuccin 'no-confirm))

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
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

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
  (defun my/google-translate-clipboard (&rest _)
    "Translate text from clipboard by using `google-translate' package."
    (interactive)
    (let ((source google-translate-default-source-language)
          (target google-translate-default-target-language)
          (output google-translate-output-destination)
          (text (gui-get-selection 'CLIPBOARD 'TEXT)))
      (google-translate-translate source target text output)))
  (bind-keys :prefix "M-o"
	     :prefix-map M-o-prefix-map
	     ("q" . google-translate-query-translate)
	     ("r" . google-translate-query-tranlate-reverse)
	     ("s" . google-translate-smooth-translate)
	     ("c" . my/google-translate-clipboard)))

(defun my/lookup-interpretation
    (&optional _ text &rest _)
  "Look up interpretation of text."
  (interactive
   (list current-prefix-arg
         (minibuffer-with-setup-hook
             (lambda (&rest _)
               (set-input-method "ukrainian-computer"))
           (read-string
            (propertize "Look up interpretation of: " 'face 'success)))))
  (let ((url "https://slovnyk.ua/index.php?swrd=%s"))
    (browse-url-with-browser-kind 'external (format url text))))
