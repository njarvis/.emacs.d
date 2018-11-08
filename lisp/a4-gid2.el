;;; a4-gid2.el --- emacs support for the "a4 gid" command -*- lexical-binding: t -*-
;;;
;;; Replacement for the original a4-gid package. Run
;;;
;;;   M-x describe-function RET a4-gid2 RET
;;;
;;; for more details.
;;;
(require 'a4-nav2)
(require 'crm)
(require 'grep)
(require 'cl-lib)

(defconst a4-gid2-name "a4-gid2")
(defconst a4-gid2-version "1beta12")
(defconst a4-gid2-default-buffer-name (format "*%s*" a4-gid2-name))

(defgroup a4-gid2 nil
  a4-gid2-name
  :group 'local)

(defcustom a4-gid2-ID-file nil
  "Name of the gid identifier database file.
Defaults to \"ID\", which \"a4 gid\" searches for backwards from
the current directory. Setting this to a filename causes \"a4
gid\" to use that file instead."
  :type '(file :must-match t)
  :risky t
  :group 'a4-gid2)

(defvar a4-gid2-default-file-type-aliases
  '(("c" . ("cpp" "h" "c" "C" "tin" "itin"))
    ("C" . ("c" "C" "h"))
    ("p" . ("py"))
    ("t" . ("tac"))))

(defcustom a4-gid2-file-type-aliases a4-gid2-default-file-type-aliases
  "File type aliases for \"a4 gid\"."
  :type '(alist :key-type string :value-type (repeat string))
  :risky t
  :group 'a4-gid2)

(defcustom a4-gid2-package-src-directory "/src"
  "Directory where packages are found for \"a4 gid\" searching."
  :type 'file
  :group 'a4-gid2)

(defcustom a4-gid2-additional-args ""
  "Additional arguments to pass to the \"a4 gid\" command."
  :type 'string
  :group 'a4-gid2)

(defcustom a4-gid2-per-target-buffer t
  "Buffer name control for a4-gid2.
If t, create a new buffer for the \"a4 gid\" output for each
target. If nil, use a single buffer, named using the
`a4-gid2-default-buffer-name' constant, for all \"a4 gid\"
output. Otherwise, the value must be a buffer name customization
function taking the \"a4 gid\" target string as an argument and
returning a buffer name string."
  :type '(choice (boolean :tag "t for per-target buffers, nil for a fixed buffer")
                 (function :tag "Target buffer name customization function"))
  :group 'a4-gid2)

(defvar a4-gid2-history ()
  "History list for a4-gid2.")

(defcustom a4-gid2-target-history-variable 'a4-gid2-history
  "History list to use for the TARGET argument of `a4-gid2' and `a4-gid2-defs'.
The value of this variable should be a symbol; that symbol is used as a
history list for \"a4 gid\" targets."
  :type 'symbol
  :group 'a4-gid2)

(defcustom a4-gid2-file-type-history-variable 'a4-gid2-history
  "History list to use for the TYPES argument of `a4-gid2' and `a4-gid2-defs'.
The value of this variable should be a symbol; that symbol is used as a
history list for \"a4 gid\" file types."
  :type 'symbol
  :group 'a4-gid2)

(defcustom a4-gid2-package-history-variable 'a4-gid2-history
  "History list to use for the PKGS argument of `a4-gid2' and `a4-gid2-defs'.
The value of this variable should be a symbol; that symbol is used as a
history list for \"a4 gid\" file types."
  :type 'symbol
  :group 'a4-gid2)

(defcustom a4-gid2-regexp-history-variable 'a4-gid2-history
  "History list to use for the REGEXPS argument of `a4-gid2' and `a4-gid2-defs'.
The value of this variable should be a symbol; that symbol is used as a
history list for \"a4 gid\" file types."
  :type 'symbol
  :group 'a4-gid2)

(defcustom a4-gid2-command-filter-functions ()
  "List of functions to filter \"a4 gid\" command strings.
Each function receives a full \"a4 gid\" command string to be
executed, and it must return either the same command string or a
new command string. If there are multiple functions in the
`a4-gid2-command-filter-functions' list, the return value from
one function becomes the input to the next."
  :type 'hook
  :group 'a4-gid2)

(defvar a4-gid2--defaults ()
  "Default values for \"a4 gid\" arguments.")

(defvar a4-gid2--command "a4 gid"
  "Command string for \"a4 gid\".
Set into a variable for testability.")

(defvar a4-gid2--cmd-history ()
  "Local per-command history list.")
(defvar a4-gid2--cmd-history-variable 'a4-gid2--cmd-history
  "Local per-command history variable.")

(defun a4-gid2--make-sep (s)
  (propertize "\0" 'display s 'separator t))

(defconst a4-gid2--comma-sep( a4-gid2--make-sep ", "))

(defun a4-gid2--target-at-point ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(defun a4-gid2--history-item-to-string (item)
  "Flatten an a4-gid2 history item to a string.
Separate the components with propertized strings."
  (concat (car item) a4-gid2--comma-sep
          (a4-gid2--make-sep "types=") (cadr item) a4-gid2--comma-sep
          (a4-gid2--make-sep "pkgs=") (cl-caddr item) a4-gid2--comma-sep
          (a4-gid2--make-sep "regexps=") (cl-cadddr item)))

(defun a4-gid2--split-on-sep (s acc)
  "Split a propertized string into an a4-gid2 history item."
  (let* ((len (length s))
         (sp (text-property-any 0 len 'separator t s)))
    (if (= len 0)
        (if (= 3 (length acc))
            (nreverse (cons "" acc))
          (nreverse acc))
      (if (not sp)
          (let ((new_acc (cons (substring-no-properties s) acc)))
            (if (= 3 (length new_acc))
                (nreverse (cons "" new_acc))
              (nreverse new_acc)))
        (let ((next_s (substring s (1+ sp) len)))
          (a4-gid2--split-on-sep
           next_s
           (if (= 0 sp)
               acc
             (cons (substring-no-properties s 0 sp) acc))))))))

(defun a4-gid2--file-type-annotations (comp)
  "Provide completion annotation strings for file type aliases."
  (let* ((all-comps (cons '("ALL" . ("all suffixes")) a4-gid2-file-type-aliases))
         (annotation (cdr-safe (assoc comp all-comps))))
    (if annotation
        (concat " (" (mapconcat 'identity annotation ", ") ")")
      "")))

(defun a4-gid2--file-type-alias-completion (str filter-func flag)
  "Perform completion for file type aliases."
  (let* ((all-comps (cons '("ALL") a4-gid2-file-type-aliases))
         (all-comps-keys (mapcar 'car all-comps)))
    (cond
     ((null flag)
      (complete-with-action nil all-comps-keys str filter-func))
     ((eq flag t)
      (complete-with-action t all-comps-keys str filter-func))
     ((eq flag 'lambda)
      (complete-with-action 'lambda all-comps-keys str filter-func))
     ((eq (car-safe flag) 'boundaries)
      nil)
     ((eq flag 'metadata)
      '(metadata . ((annotation-function . a4-gid2--file-type-annotations)))))))

(defun a4-gid2--make-arg-defaults (defs-mode)
  "Create an alist of `a4-gid2' default argument values."
  (let* ((target-default (a4-gid2--target-at-point))
         ;; create a default set of arguments based on a4-gid-defaults but
         ;; with target-default replacing the target
         (cmd-with-target
          (if a4-gid2--defaults
              (let ((newest (car a4-gid2--defaults)))
                (if (string= target-default (car newest))
                    newest
                  (list target-default (cadr newest)
                        (cl-caddr newest) (cl-cadddr newest))))))
         (history-strings
           (mapcar 'a4-gid2--history-item-to-string a4-gid2--defaults))
         (cmd-default (if cmd-with-target
                          (a4-gid2--history-item-to-string cmd-with-target)))
         (prompt (format "Target%s" (if defs-mode " definition" "")))
         (target-prompt
          (if cmd-default
              (format "%s (default %s): " prompt cmd-default)
            (format "%s: " prompt)))
         (types-default (or (cadr (car a4-gid2--defaults)) "ALL"))
         (types-prompt "File types (default %s): ")
         (pkgs-default (or (cl-caddr (car a4-gid2--defaults)) "ALL"))
         (pkgs-prompt "Packages (default %s): ")
         (regexps-default (or (cl-cadddr (car a4-gid2--defaults)) ""))
         (regexps-prompt "Regular expressions: ")
         (pkg-completions
          (let ((pkg-dir a4-gid2-package-src-directory))
            (if (file-directory-p pkg-dir)
                (append
                 (delq nil
                       (mapcar
                        #'(lambda (f)
                            (let ((p (expand-file-name f pkg-dir)))
                              (if (and (file-directory-p p)
                                       (not (string-prefix-p "." f)))
                                  f)))
                        (directory-files pkg-dir))) '("ALL"))
              '("ALL")))))
    ;; a4-gid2--cmd-history includes the target-default at the top to
    ;; facilitate the user wanting to use it while also changing some of
    ;; the other options
    (setq a4-gid2--cmd-history
          (if history-strings
              (append
               (list target-default)
               history-strings
               (symbol-value a4-gid2-target-history-variable)
               nil)))
    (list :target-prompt target-prompt
          :target-default (if cmd-default nil target-default)
          :cmd-default cmd-default
          :cmd-with-target cmd-with-target
          :types-prompt types-prompt
          :types-default types-default
          :pkgs-prompt pkgs-prompt
          :pkg-completions pkg-completions
          :pkgs-default pkgs-default
          :regexps-prompt regexps-prompt
          :regexps-default regexps-default)))

(defun a4-gid2--read-args (defs-mode)
  "Read `a4-gid2' arguments from the minibuffer.
Use defaults and history where possible."
  (let* ((history-add-new-input nil)
         (minibuffer-allow-text-properties t)
         (default-args (a4-gid2--make-arg-defaults defs-mode))
         (target-prompt (plist-get default-args :target-prompt))
         (target-default (plist-get default-args :target-default))
         (cmd-default (plist-get default-args :cmd-default))
         (target
          (read-string target-prompt target-default
                       a4-gid2--cmd-history-variable
                       (or target-default cmd-default)))
         (target-defaulted (string= target cmd-default))
         (target-with-all
          (let ((sep-tgt (a4-gid2--split-on-sep target nil)))
            (if (and (listp sep-tgt) (= 4 (length sep-tgt)))
                (progn
                  (setq target (car sep-tgt))
                  sep-tgt))))
         ;; crm-separator is the regexp separator for
         ;; completing-read-multiple
         (crm-separator " +")
         (case-all #'(lambda (s) (cond
                                  ((not (= (length s) 3)) s)
                                  ((compare-strings "all" 0 3 s 0 3 t) "ALL"))))
         (cmd-with-target (plist-get default-args :cmd-with-target))
         (types-default
          (if target-with-all
              (cadr target-with-all)
            (plist-get default-args :types-default)))
         (types-prompt
          (let ((prompt (plist-get default-args :types-prompt)))
            (format prompt types-default)))
         (types
          (funcall
           case-all
           (if target-defaulted
               (cadr cmd-with-target)
             (mapconcat
              'identity
              (completing-read-multiple
               types-prompt 'a4-gid2--file-type-alias-completion nil nil
               nil a4-gid2-file-type-history-variable types-default) " "))))
         (pkgs-default
          (if target-with-all
              (cl-caddr target-with-all)
            (plist-get default-args :pkgs-default)))
         (pkgs-prompt
          (let ((prompt (plist-get default-args :pkgs-prompt)))
            (format prompt pkgs-default)))
         (pkg-completions (plist-get default-args :pkg-completions))
         (pkgs
          (funcall
           case-all
           (if target-defaulted
               (cl-caddr cmd-with-target)
             (mapconcat
              'identity
              (completing-read-multiple
               pkgs-prompt pkg-completions nil nil
               nil a4-gid2-package-history-variable pkgs-default) " "))))
         (regexps-prompt (plist-get default-args :regexps-prompt))
         (regexps-default
          (if target-with-all
              (cl-cadddr target-with-all)
            (plist-get default-args :regexps-default)))
         (regexps
          (if target-defaulted
              (cl-cadddr cmd-with-target)
            ;; When reading regexps, we provide any regexps-default as an
            ;; initial value instead of as a default value, and we
            ;; hard-code the default to the empty string. This allows the
            ;; user to delete the initial value if they want to run a4 gid
            ;; without any regular expressions.
            (read-string regexps-prompt regexps-default
                         a4-gid2-regexp-history-variable ""))))
    (if target-defaulted
        (setq target (car cmd-with-target)))
    (if target-with-all
        (setq target (car target-with-all)))
    (let ((result (list target types pkgs regexps)))
      (add-to-history 'a4-gid2--defaults result)
      (add-to-history a4-gid2-target-history-variable target)
      (add-to-history a4-gid2-file-type-history-variable types)
      (add-to-history a4-gid2-package-history-variable pkgs)
      (add-to-history a4-gid2-regexp-history-variable regexps)
      result)))

(define-compilation-mode a4-gid2-mode "a4 gid"
  "Compilation mode for \"a4 gid\"."
  (set (make-local-variable 'compilation-error-face) grep-hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist)
       (append
        ;; ensure "a4 gid" started/finished messages aren't highlighted as
        ;; errors
        '(("^\\(a4 gid\\) \\(?:started\\|finished\\) at .+$" nil nil nil 0 1))
        grep-regexp-alist))
  ;; compilation-directory-matcher can't be nil, so we set it to a regexp that
  ;; can never match (borrowed from grep.el)
  (set (make-local-variable 'compilation-directory-matcher) '("\\`a\\`"))
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns) t)
  (set (make-local-variable 'compilation-scroll-output) 'first-error)
  (set (make-local-variable 'compilation-skip-threshold) 2)
  (add-hook 'compilation-filter-hook 'grep-filter nil t))

(defun a4-gid2--make-command (argv)
  "Make an \"a4 gid\" command string from the ARGV list.
ARGV is assumed to include all arguments, including the
command. If the `a4-gid2-command-filter-functions' list is
non-empty, call the functions it contains, passing the current
command string to each and using its results as the new command
string."
  (let ((cmd-str (mapconcat 'identity (delq nil argv) " "))
        (cmd-funs a4-gid2-command-filter-functions))
    (if cmd-funs
        (let* ((cmd-fun (car cmd-funs))
               (cmd-funs (cdr cmd-funs)))
          (while cmd-fun
            (setq cmd-str (funcall cmd-fun cmd-str))
            (setq cmd-fun (car-safe cmd-funs))
            (setq cmd-funs (cdr-safe cmd-funs)))))
    cmd-str))

(defun a4-gid2--make-buffer-name (target)
  "Make a buffer name function for \"a4 gid\" results."
  #'(lambda (mode)
      (cond
       ((eq a4-gid2-per-target-buffer nil) a4-gid2-default-buffer-name)
       ((eq a4-gid2-per-target-buffer t) (format "*%s %s*" mode target))
       (t (funcall a4-gid2-per-target-buffer target)))))

(defun a4-gid2--compilation-exit-message (status code msg)
  (if (eq status 'exit)
      ;; This relies on the fact that `compilation-start'
      ;; sets buffer-modified to nil before running the command,
      ;; so the buffer is still unmodified if there is no output.
      (cond ((and (zerop code) (buffer-modified-p))
             '("finished (matches found)\n" . "matched"))
            ((not (buffer-modified-p))
             '("finished with no matches found\n" . "no match"))
            (t
             (cons msg code)))
    (cons msg code)))

(defun a4-gid2--compile (target types pkgs regexps defs)
  "Execute \"a4 gid\" in its compilation-mode with the given arguments."
  (a4-nav2-add (point-marker))
  (let* ((tgt-opt (format "-g %s" (shell-quote-argument target)))
         (types-opt (format "-t %s" (shell-quote-argument types)))
         (pkgs-opt (format "-p %s" (shell-quote-argument pkgs)))
         (regexps-opt (if (not (string= regexps ""))
                          (format "-r %s" (shell-quote-argument regexps))))
         (defs-opt (if defs "-D"))
         (file-opt (if a4-gid2-ID-file (format "-f %s"a4-gid2-ID-file)))
         (argv (list a4-gid2--command tgt-opt types-opt
                     pkgs-opt regexps-opt defs-opt file-opt
                     a4-gid2-additional-args))
         (cmd (a4-gid2--make-command argv))
         (buffer-name-function (a4-gid2--make-buffer-name target))
         (compilation-exit-message-function 'a4-gid2--compilation-exit-message))
    (compilation-start cmd 'a4-gid2-mode buffer-name-function)))

(defun a4-gid2 (target file-types packages regexps)
  "Run \"a4 gid\" to search source code for TARGET.

`a4-gid2' searches for all occurrences of the TARGET string.

If FILE-TYPES is specified, limit the search to files with the
specified suffixes. Suffix group aliases defined in the variable
`a4-gid2-default-file-type-aliases' may be used instead of actual
suffixes. FILE-TYPES defaults to all file types supported by
gid. To specify FILE-TYPES, supply a space-separated list of
suffixes or aliases as a string. In interactive mode, `a4-gid2'
performs completion for FILE-TYPES using aliases specified in
`a4-gid2-default-file-type-aliases'.

If PACKAGES is specified, limit the search to files in only the
specified packages. PACKAGES defaults to all packages. To specify
PACKAGES, supply a space-separated package names as a string. In
interactive mode, `a4-gid2' performs completion for package
names.

If REGEXPS is specified, filter the search results to those
matching the specified regular expression patterns. To specify
REGEXPS, supply a space-separated list of Python-style regular
expressions as a string.

In interactive mode, `a4-gid2' works with regular minibuffer
history, but it also temporarily inserts TARGET into available
history to allow you to easily modify optional parameters when
you don't want to accept command defaults. See Info node
`Minibuffer History' for more details on history navigation and
usage.

By default, output from \"a4 gid\" goes into a per-target
buffer. You can control this via the `a4-gid2-per-target-buffer'
variable.

\"a4 gid\" expects that you previously generated an ID file via `a4
mkid' or that you specify an ID file via the `a4-gid2-ID-file'
variable.

You may specify additional arguments to pass to \"a4 gid\" in the
`a4-gid2-additional-args' variable."
  (interactive (a4-gid2--read-args nil))
  (a4-gid2--compile target file-types packages regexps nil))

(defun a4-gid2-defs (target file-types packages regexps)
  "Run \"a4 gid\" to search source code for the TARGET definition.

Same as `a4-gid2' but search for the definition of TARGET instead
of searching for occurrences of TARGET. See `a4-gid2' for more
details."
  (interactive (a4-gid2--read-args t))
  (a4-gid2--compile target file-types packages regexps t))

(provide 'a4-gid2)
