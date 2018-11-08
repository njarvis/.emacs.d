(cond ((equal company "arista")
       (if (require 'a4-gid2 nil 'noerror)
           (progn
             ;; Always use /src/ID built by "a4 mkid", rather than searching upwards
             ;; from the current directory. Better for when a4-gid invoked outside /src.
             (setq a4-gid2-additional-args "-f /src/ID")
             (global-set-key (kbd "M-\r") (quote a4-gid2)))
         (progn
           ;; Always use /src/ID built by "a4 mkid", rather than searching upwards
           ;; from the current directory. Better for when a4-gid invoked outside /src.
           (setq a4-gid-additional-args "-f /src/ID")
           (global-set-key (kbd "M-}") (quote a4-gid-defs))
           (global-set-key (kbd "M-#") (quote a4-gid-defs-repeat))
           (global-set-key (kbd "M-\r") (quote a4-gid))
           (global-set-key (kbd "M-]") (quote a4-gid-repeat))
           (global-set-key (kbd "M-'") (quote a4-gid-go-back))
           (global-set-key (kbd "M-;") (quote a4-gid-kill)))))
      (t
       (autoload 'gid "idutils" nil t)
       (global-set-key "\M-\r" 'gid)))



