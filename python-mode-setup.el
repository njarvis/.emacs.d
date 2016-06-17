(require 'python)
(add-hook 'python-mode-hook (lambda ()
			      (require 'sphinx-doc)
			      (sphinx-doc-mode t)))
(add-hook 'python-mode-hook (lambda ()
			      (require 'ws-butler)
			      (ws-butler-mode)))


