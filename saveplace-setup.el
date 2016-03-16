;; saveplace: save location in file when saving files
(require 'saveplace)                   ;; get the package
(setq
 save-place-file (format "~/.emacs.d/cache/saveplace.%s" (system-name))
 save-place-limit 10)
(setq-default save-place t)            ;; activate it for all buffers

(if (>= emacs-major-version '22)
    ;;
    ;; savehist: save some history
    (require 'savehist)
    (setq savehist-additional-variables    ;; also save...
	  '(search ring regexp-search-ring);; ... my search entries
	  savehist-autosave-interval 60    ;; save every minute (default: 5 min)
	  savehist-file (format "~/.emacs.d/cache/savehist.%s" (system-name))) ;; keep my home clean
    (savehist-mode t)                ;; do customization before activation
    )

