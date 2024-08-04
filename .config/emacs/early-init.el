;;; early-init.el --- Early configuration. -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
	  (lambda (&rest _)
	    (setq gc-cons-threshold (expt 2 23))))

(setq package-enable-at-startup nil)

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent))

(startup-redirect-eln-cache (expand-file-name "tmp/eln-cache/" user-emacs-directory))

(customize-set-variable 'frame-resize-pixelwise t)

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(horizontal-scroll-bars . nil))

(add-to-list 'default-frame-alist '(menu-bar-lines . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines nil))

;; (add-to-list 'initial-frame-alist '(visibility . nil))
;; (add-hook 'window-setup-hook 'make-frame-visible)
