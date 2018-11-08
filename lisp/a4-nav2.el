;;; a4-nav2.el --- marker navigation for Arista packages -*- lexical-binding: t -*-
;;;
;;; Replacement for the original a4-nav package. Uses regular ring and
;;; marker support.
;;;
(require 'ring)

(defconst a4-nav2-version "1beta4")

(defgroup a4-nav2 nil
  "a4-nav2"
  :version "24.5"
  :group 'local)

(defvar a4-nav2--ring nil)
(defvar a4-nav2--ring-idx -1)

(defun a4-nav2--set-new-ring-size (sym size)
  (if (<= size 0)
      (error "a4-nav2 ring size must be greater than 0"))
  (if (not a4-nav2--ring)
      (progn
        (setq a4-nav2--ring (make-ring size))
        (setq a4-nav2--ring-idx -1))
    (let ((old-size (ring-size a4-nav2--ring)))
      (if (not (= size old-size))
          (let ((new-ring (make-ring size))
                (idx (1- (min size (ring-length a4-nav2--ring)))))
            (while (>= idx 0)
              (ring-insert new-ring (ring-remove a4-nav2--ring idx))
              (setq idx (1- idx)))
            (while (not (ring-empty-p a4-nav2--ring))
              (set-marker (ring-remove a4-nav2--ring) nil nil))
            (setq a4-nav2--ring new-ring))))
    (if (>= a4-nav2--ring-idx size)
        (setq a4-nav2--ring-idx 0)))
  (set sym size))

(defcustom a4-nav2-ring-size 16
  "User-specified default size of a4-nav2 ring."
  :type 'integer
  :risky t
  :set 'a4-nav2--set-new-ring-size
  :initialize 'custom-initialize-set
  :require 'a4-nav2
  :version "24.5"
  :group 'a4-nav2)

(defcustom a4-nav2-display-buffer-base-action nil
  "User-specified default action for how a4-nav2 displays buffers.
It should be a cons cell (FUNCTION . ALIST), where FUNCTION is a
function or a list of functions.  Each function should accept two
arguments: a buffer to display and an alist similar to ALIST.
See ‘display-buffer’ for details."
  :type display-buffer--action-custom-type
  :risky t
  :version "24.5"
  :group 'a4-nav2)

(defun a4-nav2--delete-index (idx)
  "Delete item at index IDX in a4-nav2 ring."
  (ring-remove a4-nav2--ring idx)
  (let ((len (ring-length a4-nav2--ring)))
    (cond
     ((= len 0) (setq a4-nav2--ring-idx -1))
     ((> a4-nav2--ring-idx len) (setq a4-nav2--ring-idx 0))
     (t a4-nav2--ring-idx))))

(defun a4-nav2-kill-buffer-hook ()
  "Hook function for removing killed buffers from a4-nav2 ring."
  (if (and a4-nav2--ring (not (ring-empty-p a4-nav2--ring)))
      (let ((buf (current-buffer))
            (cur)
            (idx 0)
            (len (ring-length a4-nav2--ring)))
        (while (< idx len)
          (setq cur (ring-ref a4-nav2--ring idx))
          (if (eq buf (marker-buffer cur))
              (progn
                (a4-nav2--delete-index idx)
                (set-marker cur nil)))
          (setq idx (1+ idx))))))
(add-hook 'kill-buffer-hook 'a4-nav2-kill-buffer-hook)

(defun a4-nav2-reset ()
  "Remove all buffers and positions from the a4-nav2 ring."
  (interactive)
  (if (not a4-nav2--ring)
      (a4-nav2--set-new-ring-size 'a4-nav2-ring-size a4-nav2-ring-size)
    (while (not (ring-empty-p a4-nav2--ring))
      (let ((m (ring-ref a4-nav2--ring 0)))
        (set-marker m nil nil)
        (ring-remove a4-nav2--ring 0)))
    (setq a4-nav2--ring-idx -1)))

(defun a4-nav2-add (m)
  "Add marker M to the a4-nav2 ring.
M defaults to the current buffer and position.  If the most
recently added buffer and position are identical to M, don't add
them again. Set ring navigation to the newly-added marker."
  (interactive (list (point-marker)))
  (if (not a4-nav2--ring)
      (a4-nav2--set-new-ring-size 'a4-nav2-ring-size a4-nav2-ring-size))
  (if (or (ring-empty-p a4-nav2--ring)
          (not (equal m (ring-ref a4-nav2--ring 0))))
      (ring-insert a4-nav2--ring m))
  (setq a4-nav2--ring-idx 0))

(defun a4-nav2-delete (m)
  "Remove marker M from the a4-nav2 ring.
M defaults to the current buffer and position. If ring navigation
points to the marker being removed, move it to the next marker if
any."
  (interactive (list (point-marker)))
  (if a4-nav2--ring
      (if (not (ring-empty-p a4-nav2--ring))
          (let ((idx 0)
                (done nil)
                (len (ring-length a4-nav2--ring)))
            (while (not done)
              (let ((cur (ring-ref a4-nav2--ring idx)))
                (if (not (equal m cur))
                    (if (= (setq idx (1+ idx)) len)
                        (setq done t))
                  (a4-nav2--delete-index idx)
                  (set-marker cur nil nil)
                  (setq done t))))))))

(defun a4-nav2--goto-idx ()
  (if (and a4-nav2--ring (not (= a4-nav2--ring-idx -1)))
      (let* ((m (ring-ref a4-nav2--ring a4-nav2--ring-idx))
             (buf (marker-buffer m))
             (pos (marker-position m)))
        (pop-to-buffer buf a4-nav2-display-buffer-base-action)
        (goto-char pos))))

(defun a4-nav2-next ()
  "Go to the next buffer and position in the a4-nav2 ring."
  (interactive)
  (if (or (not a4-nav2--ring) (ring-empty-p a4-nav2--ring))
      (error "a4-nav2 ring is empty")
    (let ((len (ring-length a4-nav2--ring)))
      (if (>= (setq a4-nav2--ring-idx (1+ a4-nav2--ring-idx)) len)
          (setq a4-nav2--ring-idx 0))
      (a4-nav2--goto-idx))))

(defun a4-nav2-prev ()
  "Go to the previous buffer and position in the a4-nav2 ring."
  (interactive)
  (if (or (not a4-nav2--ring) (ring-empty-p a4-nav2--ring))
      (error "a4-nav2 ring is empty")
    (let ((len (ring-length a4-nav2--ring)))
      (if (< (setq a4-nav2--ring-idx (1- a4-nav2--ring-idx)) 0)
          (setq a4-nav2--ring-idx (1- len)))
      (a4-nav2--goto-idx))))

(provide 'a4-nav2)
