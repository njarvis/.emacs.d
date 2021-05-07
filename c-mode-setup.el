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
            (c-add-style "linux-tabs-only" linux-c-style)
	    ;; Enable whitespace butler
	    (require 'ws-butler)
	    (ws-butler-mode)
	    (cond ((equal company "senient")
		   (message "Senient c-mode")
		   (c-add-style "Senient" senient-c-style))
		  ((equal company "arista")
                   (setq fill-column 85)
		   (arastra-c-mode-common-hook)))))

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
		(cond ((equal company "senient")
		       (message "Senient mode: %s" filename)
		       (c-set-style "Senient")
		       (setq indent-tabs-mode nil))
		      ((equal company "arista")
		       (message "Arista mode: %s" filename)
		       (c-set-style "Arastra")))
		))))
