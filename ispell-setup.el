;; spell

(autoload 'ispell-word "ispell.el")

(cond
   ((string-equal system-type "darwin") ; Mac OS X
    (progn (message "Mac OS X Spelling"))
    (add-to-list 'exec-path "/usr/local/bin")
    (setq ispell-program-name "aspell"
	  ispell-local-dictionary "british"
	  ispell-dictionary "british"
	  ispell-dictionary-alist
	  (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
			   ("-B" "-d" "english" "--dict-dir"
			    "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
			   nil iso-8859-1)))
	    `((nil ,@default)
	      ("british" ,@default))))
    )
)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'c-mode-common-hook
	  'flyspell-prog-mode)
