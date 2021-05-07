(if (file-directory-p (expand-file-name "~/.emacs.d/Arista"))
  (progn (message "Arista Company setup: using private copy")
         (add-to-list 'load-path (expand-file-name "~/.emacs.d/Arista")))
  (message "Arista Company setup: using system copy"))

(setq arastra-ignore-libs '("nifty-buffer" "nifty-file"))
(load-library "Arastra")

;; When editing submit notes, automatically jump to the description
(define-derived-mode arastra-p4-submit-mode text-mode "P4"
  "A mode for setting up with-editor-minor mode and the point when editing a p4 submit description"
;  (with-editor-mode)
  (beginning-of-buffer)
  (search-forward "<enter description here>")
  (search-backward "<")
  (kill-line)
  )
(add-to-list 'magic-mode-alist '("# A Perforce Change Specification" . arastra-p4-submit-mode))
