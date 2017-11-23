;; Save the starting time...
(defvar *init-start-time* (current-time))

;; package setup
(load-file "~/.emacs.d/package-setup.el")

;; Packages we want to be installed, but don't require a setup.el file
(ensure-package-installed 'cmake-mode 'sphinx-doc 'ws-butler 'groovy-mode 'fill-column-indicator 'exec-path-from-shell)

;; Company specific setup
(setq company "arista")

(cond
 ((equal company "senient") (load-file "~/.emacs.d/senient-setup.el"))
 ((equal company "arista") (load-file "~/.emacs.d/arista-setup.el"))
 (t (progn (message "No Company setup"))))

;; backup setup
(load-file "~/.emacs.d/backup-setup.el")

;; c-mode setup
(load-file "~/.emacs.d/c-mode-setup.el")

;; java-mode setup
(load-file "~/.emacs.d/java-mode-setup.el")

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

;; python mode setup
(load-file "~/.emacs.d/python-mode-setup.el")

;; protobuf mode setup
(load-file "~/.emacs.d/protobuf-mode-setup.el")

;; yaml setup
(load-file "~/.emacs.d/yaml-mode-setup.el")

;; kite-setup
;;(load-file "~/.emacs.d/kite.el")

;; Display mode
(line-number-mode t)
(column-number-mode t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell yaml-mode ws-butler sphinx-doc protobuf-mode markdown-mode groovy-mode fill-column-indicator cmake-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
