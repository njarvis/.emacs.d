;; saveplace: save location in file when saving files
(require 'saveplace)                   ;; get the package
(setq
 save-place-file "~/.emacs.d/cache/saveplace"
 save-place-limit 10)
(setq-default save-place t)            ;; activate it for all buffers

;;
;; savehist: save some history
(require 'savehist)
(setq savehist-additional-variables    ;; also save...
      '(search ring regexp-search-ring);; ... my search entries
      savehist-autosave-interval 60    ;; save every minute (default: 5 min)
      savehist-file "~/.emacs.d/cache/savehist") ;; keep my home clean
(savehist-mode t)                ;; do customization before activation


