
(defconst senient-java-style
  '((c-basic-offset . 2)
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
  "Senient JAVA Style for CC-MODE")

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add Senient Java style
            (c-add-style
             "Senient-Java" senient-java-style)))

(add-hook 'java-mode-hook
	  (lambda ()
	    (let ((filename (buffer-file-name)))
	      (message "Senient mode: %s" filename)
	      (c-set-style "Senient-Java")
	      (setq indent-tabs-mode nil))))
