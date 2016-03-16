(defconst senient-c-style
  '((c-basic-offset . 4)
    (c-auto-newline)
    (comment-multi-line t)
    (c-offsets-alist
     (knr-argdecl-intro . +)
     (knr-argdecl . 0)
     (statement-cont . +)
     (statement-case-open . +)
     (substatement-open . 0)
     (label . 0)
     (case-label . 0)))
  "Senient C Style for CC-MODE")

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add Senient style
            (c-add-style
             "Senient" senient-c-style)))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defconst linux-c-style
  '((c-basic-offset . 8)
    (indent-tabs-mode . t)
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only)))
  "Linux C Style for CC-MODE")

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only" linux-c-style)))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (if (and filename
                         (or 
			  (string-match "/kernel" filename)
			  (locate-dominating-file filename "Kbuild")))
		(progn
		  (message "Kernel mode: %s" filename)
		  (c-set-style "linux-tabs-only"))
		(progn
		  (message "Senient mode: %s" filename)
		  (c-set-style "Senient")
		  (setq indent-tabs-mode nil))))))
