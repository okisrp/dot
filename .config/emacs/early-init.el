;;; early-init.el --- Early configuration. -*- lexical-binding: t -*-

;; Increase startup speed.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
		  (lambda (&rest _)
			(setq gc-cons-threshold (expt 2 23))))

;; Prevent `package.el' from loading packages prior to their init-file
;; loading.
(setq package-enable-at-startup nil)

;; Put native compilation errors and warnings to separate buffer that
;; won't pop up all of sudden.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Treat a frame by pixels not by its char width and height.
(customize-set-variable 'frame-resize-pixelwise t)

;; Remove tool, menu and scroll bars from all frames.
(dolist (prop '(vertical-scroll-bars
				horizontal-scroll-bars
				menu-bar-lines
				tool-bar-lines))
  (add-to-list 'default-frame-alist `(,prop)))

;; NOTE: If you're using `DOOM-emacs' framework that put these lines
;; at the begining of `$DOOMDIR/init.el' file.
;; (add-to-list 'initial-frame-alist '(visibility . nil))
;; (add-hook 'window-setup-hook 'make-frame-visible)

;; Move `user-emacs-directory' to cache folder. If you are trying to
;; keep your configuration clean than that's what you really need.
(when-let ((dir (getenv "XDG_CACHE_HOME")))
  (defvar old-user-emacs-dir user-emacs-directory
	"Store the default value of `user-emacs-directory' variable.")
  (setq user-emacs-directory (expand-file-name "emacs/" dir)))

;; Move backup files to specific directory.
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
      make-backup-files t)

;; Move lock files to particular folder.
(let ((dir (expand-file-name "lock-files/" user-emacs-directory)))
  (unless (file-directory-p dir)
    (make-directory dir 'parents))
  (setq lock-file-name-transforms `((".*" ,dir t))
		create-lockfiles t))
