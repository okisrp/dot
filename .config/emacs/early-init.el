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
(dolist (property '((vertical-scroll-bars . nil)
					(horizontal-scroll-bars . nil)
					(menu-bar-lines . nil)
					(tool-bar-lines . nil)))
  (add-to-list 'default-frame-alist `(,(car property) . ,(cdr property))))

;; NOTE: If you're using `DOOM-emacs' framework that put these lines
;; at the begining of `$DOOMDIR/init.el' file.
;; (add-to-list 'initial-frame-alist '(visibility . nil))
;; (add-hook 'window-setup-hook 'make-frame-visible)
