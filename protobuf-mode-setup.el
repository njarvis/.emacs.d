(ensure-package-installed 'protobuf-mode)
(when (< emacs-major-version 27)
  (require 'cl))
(require 'protobuf-mode)

