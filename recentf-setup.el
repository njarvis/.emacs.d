(require 'recentf)
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(setq recentf-save-file "~/.emacs.d/cache/recentf"
      recentf-max-saved-items 100     ;; max save 100
      recentf-max-menu-items 15)      ;; max 15 in menu
(global-set-key "\C-x\C-r" 'recentf-open-files)
(recentf-mode t)
