(when (file-directory-p (expand-file-name "~/.emacs.d/Senient"))
  (progn (message "Senient Company setup"))
  
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/Senient"))

  ;; Makes TAB do the right thing for various languages.
  (load-library "senient-indent")
)


		      
