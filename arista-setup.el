(if (file-directory-p (expand-file-name "~/.emacs.d/Arista"))
  (progn (message "Arista Company setup: using private copy")
         (add-to-list 'load-path (expand-file-name "~/.emacs.d/Arista")))
  (message "Arista Company setup: using system copy"))

;; Makes TAB do the right thing for various languages.
(load-library "arastra-indent")

;; Integration with a4, for checking files out and for
;; compiling/running/debugging within emacs.
(load-library "a4")

;; Makes C-v and M-v do what they should, rather than what Stallman wanted;
;; specifically, makes C-v and M-v inverses of each other so pressing one
;; and then the other is always a no-op.
(if (featurep 'xemacs)
    (load-library "scroll-in-place"))

;; Makes M-k switch to a recently used buffer, with lightning
;; completion.  Wrapper for iswitchb
;;(load-library "nifty-buffer")

;; Gives you "C-x f" to find file at point.  Useful for jumping to
;; errors, etc.
(load-library "nifty-file")

;; Gives you C-z and M-z for running shells inside emacs.  Emacs is good
;; for you.
(load-library "ashell")

;; Gives you M-e and M-r for sane defining and running of keyboard
;; macros.
(load-library "keyboard-macros")

;; Some little useful functions like C-ct for a timestamp
(load-library "arastra-utils")

;; Dynamic completion of words with Meta-return
;;(autoload 'dabbrev-expand "dabbrev" "Expand previous word \"dynamically\".")

;; Support for gnu id-utils
(autoload 'gid "id-utils" "Lookup a string in a mkid database" t)

;; Support for coloring buffer regions
(autoload 'colorize "colorize" "Supporting for coloring regions" t)

;; Other improvements (IMO) over default emacs keybindings.
(load-library "misc-bindings")

;; Arista gid extensions
(if (locate-library "a4-gid2")
    (load-library "a4-gid2")
  (load-library "a4-gid"))

;; Have gnuclient always use the same frame if not running in X
;; emacsclient does this by default.
(if (featurep 'xemacs)
    (if (eq (frame-type) 'tty)
        (setq gnuserv-frame t)))

(require 'aformat-mode)

;; Disable fsync
(setq write-region-inhibit-fsync t)

;; Hook to enable pdb-track under emacs24.x
(add-hook 'comint-output-filter-functions
          'python-pdbtrack-comint-output-filter-function)

;; Golang support.
(load-library "go-mode")

;; If it is installed, use goimports over gofmt as the format command
(if (= (call-process "which" nil nil nil "goimports") 0)
    (setq gofmt-command "goimports"))
(add-hook 'before-save-hook 'gofmt-before-save)

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
