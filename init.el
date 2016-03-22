;; Save the starting time...
(defvar *init-start-time* (current-time))

;; package setup
(load-file "~/.emacs.d/package-setup.el")

;; backup setup
(load-file "~/.emacs.d/backup-setup.el")

;; c-mode setup
(load-file "~/.emacs.d/c-mode-setup.el")

;; compile setup
(load-file "~/.emacs.d/compile-setup.el")

;; gid setup
(load-file "~/.emacs.d/gid-setup.el")

;; recentf setup
(load-file "~/.emacs.d/recentf-setup.el")

;; saveplace setup
(load-file "~/.emacs.d/saveplace-setup.el")

;; ispell setup
(load-file "~/.emacs.d/ispell-setup.el")

;; tramp setup
(load-file "~/.emacs.d/tramp-setup.el")

;; auto-mode-alist setup
(load-file "~/.emacs.d/auto-mode-alist-setup.el")

;; keyboard setup
(load-file "~/.emacs.d/keyboard-setup.el")

;; markdown mode setup
(load-file "~/.emacs.d/markdown-mode-setup.el")

;; Display mode
(line-number-mode t)
(column-number-mode t)

;; Packages we want to be installed, but don't require a setup.el file
(ensure-package-installed 'cmake-mode 'protobuf-mode)

;; Finalise package installation
(package-initialize)

;; OS Specific setup
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  ;; http://superuser.com/questions/277956/emacs-variable-to-open-with-in-original-frame
  (setq ns-pop-up-frames nil))

;; Get the end time.
(defvar *init-end-time* (current-time))

;; Print init time.
(message "Init took %d seconds"
         (- (+ (lsh (car *init-end-time*) 16) (cadr *init-end-time*))
	    (+ (lsh (car *init-start-time*) 16) (cadr
						 *init-start-time*))))

