;;; dired-numbering.el --- Reorder and number files in dired, e.g. CD tracks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ioannis Canellos

;; Author: Ioannis Canellos <iocanel@gmail.com>
;; Maintainer: Ioannis Canellos <iocanel@gmail.com>
;; URL: https://github.com/iocanel/dired-numbering.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: files dired convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A minor mode for reordering and numbering the files of a Dired buffer,
;; handy for preparing the track order of an audio (mp3) CD.
;;
;; Enabling `dired-numbering-mode' turns the buffer editable via wdired but
;; does not touch any filename, so opening a directory is always safe.
;; Numbers are zero-padded ("01 - name.mp3") and the padding grows with the
;; number of files.
;;
;; - `dired-numbering-move-up' / `dired-numbering-move-down' (M-<up> /
;;   M-<down>) move the file one position.  The file swaps its number with its
;;   neighbour and the two lines swap places, so the buffer always reads in
;;   numeric order.  Files that are not numbered yet get numbered first.
;; - `dired-numbering-renumber' (C-c C-r) closes gaps (01,02,05 becomes
;;   01,02,03) while keeping the order the numbers express.
;; - C-c C-c commits the renames to disk (wdired), C-c C-k aborts.
;;
;; A base name is always welded to its file: moving a track only ever changes
;; its numeric prefix, never the name that follows it.  While the mode is on,
;; committing refuses any rename that would change more than the number.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'seq)
(require 'wdired)

(defgroup dired-numbering nil
  "Reorder and number files in Dired."
  :group 'dired
  :prefix "dired-numbering-")

(defcustom dired-numbering-separator " - "
  "String placed between the number and the base name."
  :type 'string
  :group 'dired-numbering)

(defcustom dired-numbering-min-width 2
  "Minimum number of digits used for the numeric prefix."
  :type 'integer
  :group 'dired-numbering)

(defun dired-numbering--prefix-regexp ()
  "Return a regexp matching an existing numeric prefix on a filename."
  (concat "\\`\\([0-9]+\\)" (regexp-quote dired-numbering-separator)))

(defun dired-numbering--file-on-line ()
  "Return the basename of the file on the current line, or nil.
The . and .. entries and non-file lines return nil."
  (let ((name (dired-get-filename 'no-dir t)))
    (and name (not (member name '("." ".."))) name)))

(defun dired-numbering--prefix (name)
  "Return the numeric prefix of NAME, such as \"03 - \", or nil when it has none."
  (and name
       (string-match (dired-numbering--prefix-regexp) name)
       (match-string 0 name)))

(defun dired-numbering--number (name)
  "Return the number at the start of NAME, or nil when it has none."
  (and name
       (string-match (dired-numbering--prefix-regexp) name)
       (string-to-number (match-string 1 name))))

(defun dired-numbering--bare (name)
  "Return NAME without its numeric prefix."
  (and name (replace-regexp-in-string (dired-numbering--prefix-regexp) "" name)))

(defun dired-numbering--filename-region ()
  "Return (BEG . END) of the editable filename on this line, or nil.
END comes from `dired-move-to-end-of-filename' rather than the end of the
line, so the target of a symlink line is left alone."
  (save-excursion
    (beginning-of-line)
    (when (and (dired-numbering--file-on-line)
               (dired-move-to-filename))
      (let ((beg (point)))
        (cons beg (or (ignore-errors (dired-move-to-end-of-filename t) (point))
                      (line-end-position)))))))

(defun dired-numbering--set-filename (new)
  "Replace the filename on the current line with NEW."
  (let ((region (dired-numbering--filename-region)))
    (when region
      (goto-char (car region))
      (delete-region (car region) (cdr region))
      (insert new))))

(defun dired-numbering--file-names ()
  "Return the basenames of all file lines, in buffer order."
  (let ((names nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((name (dired-numbering--file-on-line)))
          (when name (push name names)))
        (forward-line 1)))
    (nreverse names)))

(defun dired-numbering--width (count)
  "Return the digits needed to number COUNT files."
  (max dired-numbering-min-width (length (number-to-string count))))

(defun dired-numbering--in-order-p ()
  "Non-nil when every file is numbered and the buffer lists them in order.
Moving a file relies on this: it swaps numbers with the neighbouring line,
which is only correct when the neighbouring line is also the numeric
neighbour."
  (let ((numbers (mapcar #'dired-numbering--number (dired-numbering--file-names))))
    (and (not (memq nil numbers))
         (or (null numbers) (apply #'< numbers)))))

(defun dired-numbering--goto-file (name)
  "Put point on the filename of the line whose basename is NAME.
Return non-nil when found."
  (let ((found nil))
    (goto-char (point-min))
    (while (and (not found) (not (eobp)))
      (if (equal (dired-numbering--file-on-line) name)
          (setq found t)
        (forward-line 1)))
    (when found (dired-move-to-filename) t)))

(defun dired-numbering--goto-next-file-line (dir)
  "Move to the next (DIR=1) or previous (DIR=-1) file line.
Return non-nil on success, nil when there is none."
  (let ((found nil) (moved t))
    (while (and (not found) moved)
      (setq moved (zerop (forward-line dir)))
      (when (and moved (dired-numbering--file-on-line)) (setq found t)))
    found))

(defun dired-numbering--line-text (pos)
  "Return the whole line at POS, trailing newline and text properties included."
  (save-excursion
    (goto-char pos)
    (buffer-substring (line-beginning-position)
                      (min (point-max) (1+ (line-end-position))))))

(defun dired-numbering--replace-line (pos text)
  "Replace the whole line at POS with TEXT."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char pos)
      (delete-region (line-beginning-position)
                     (min (point-max) (1+ (line-end-position))))
      (insert text))))

(defun dired-numbering--ensure-editable ()
  "Switch the buffer back to wdired when a commit or abort left it in Dired."
  (unless (derived-mode-p 'wdired-mode)
    (wdired-change-to-wdired-mode)))

(defun dired-numbering--renumber ()
  "Number every file 1..N and list the lines in that order.
Files keep the order their current numbers express, unnumbered ones go last
in buffer order.  Return the number of files."
  (let ((entries nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((name (dired-numbering--file-on-line)))
          (when name
            (push (list (or (dired-numbering--number name) most-positive-fixnum)
                        (copy-marker (line-beginning-position))
                        name)
                  entries)))
        (forward-line 1)))
    (setq entries (nreverse entries))
    (let* ((slots (mapcar #'cadr entries))
           (sorted (sort (copy-sequence entries)
                         (lambda (x y) (< (car x) (car y)))))
           (fmt (format "%%0%dd%s%%s"
                        (dired-numbering--width (length entries))
                        (replace-regexp-in-string "%" "%%" dired-numbering-separator)))
           (index 0))
      (save-excursion
        (dolist (entry sorted)
          (setq index (1+ index))
          (let ((new (format fmt index (dired-numbering--bare (nth 2 entry)))))
            (unless (string= (nth 2 entry) new)
              (goto-char (nth 1 entry))
              (dired-numbering--set-filename new))))
        ;; Lines travel whole, so each name stays with its own metadata and the
        ;; wdired properties that identify the file on disk.  Filling the slots
        ;; bottom up keeps the positions of the slots above valid.
        (let ((texts (mapcar (lambda (entry) (dired-numbering--line-text (nth 1 entry)))
                             sorted)))
          (unless (equal sorted entries)
            (cl-loop for slot in (reverse slots)
                     for text in (reverse texts)
                     do (dired-numbering--replace-line slot text)))))
      (dolist (entry entries) (set-marker (nth 1 entry) nil))
      (length entries))))

(defun dired-numbering--renumber-keeping-point ()
  "Renumber every file, keeping point on the same file."
  (let ((bare (dired-numbering--bare (dired-numbering--file-on-line)))
        (count (dired-numbering--renumber)))
    (when bare
      (dired-numbering--goto-file
       (seq-find (lambda (name) (equal (dired-numbering--bare name) bare))
                 (dired-numbering--file-names))))
    (message "Numbered %d files" count)))

(defun dired-numbering--move (dir)
  "Move the file on the current line one position in DIR (1 or -1).
Only the numeric prefixes of the two lines are exchanged, so each base name
stays with its own file, then the two lines swap places so the buffer keeps
reading in numeric order."
  (dired-numbering--ensure-editable)
  (unless (dired-numbering--file-on-line)
    (user-error "Not on a file line"))
  (unless (dired-numbering--in-order-p)
    (dired-numbering--renumber-keeping-point))
  (let* ((a-name (dired-numbering--file-on-line))
         (a-marker (copy-marker (line-beginning-position)))
         (b-name nil)
         (b-marker nil))
    (save-excursion
      (when (dired-numbering--goto-next-file-line dir)
        (setq b-name (dired-numbering--file-on-line)
              b-marker (copy-marker (line-beginning-position)))))
    (if (not b-name)
        (message "Already at the %s" (if (< dir 0) "top" "bottom"))
      (let ((a-prefix (dired-numbering--prefix a-name))
            (b-prefix (dired-numbering--prefix b-name))
            (moved (concat (dired-numbering--prefix b-name)
                           (dired-numbering--bare a-name))))
        (save-excursion
          (goto-char b-marker)
          (dired-numbering--set-filename (concat a-prefix (dired-numbering--bare b-name)))
          (goto-char a-marker)
          (dired-numbering--set-filename (concat b-prefix (dired-numbering--bare a-name))))
        (let* ((upper (min a-marker b-marker))
               (lower (max a-marker b-marker))
               (upper-text (dired-numbering--line-text upper))
               (lower-text (dired-numbering--line-text lower)))
          (dired-numbering--replace-line lower upper-text)
          (dired-numbering--replace-line upper lower-text))
        ;; The swap relocated the line, so follow the file by name: a stale
        ;; position would make the next move act on the wrong file.
        (dired-numbering--goto-file moved))
      (set-marker b-marker nil))
    (set-marker a-marker nil)))

;;;###autoload
(defun dired-numbering-move-up ()
  "Move the file on the current line up one position."
  (interactive)
  (dired-numbering--move -1))

;;;###autoload
(defun dired-numbering-move-down ()
  "Move the file on the current line down one position."
  (interactive)
  (dired-numbering--move 1))

;;;###autoload
(defun dired-numbering-renumber ()
  "Number every file 1..N, closing gaps in the existing numbering.
Files are sorted by their current number, not by their position in the
buffer, so an order built with the move commands is kept.  Unnumbered files
go last."
  (interactive)
  (dired-numbering--ensure-editable)
  (dired-numbering--renumber-keeping-point))

(defvar dired-numbering-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-<up>") #'dired-numbering-move-up)
    (define-key map (kbd "M-<down>") #'dired-numbering-move-down)
    (define-key map (kbd "C-c C-r") #'dired-numbering-renumber)
    map)
  "Keymap for `dired-numbering-mode'.")

;;;###autoload
(define-minor-mode dired-numbering-mode
  "Reorder and number the files of a Dired buffer.
The buffer becomes editable via wdired.  Commit the renames with
\\[wdired-finish-edit] or abort with \\[wdired-abort-changes].  The move
commands switch back to wdired on their own after a commit or an abort.

\\{dired-numbering-mode-map}"
  :lighter " Num"
  :keymap dired-numbering-mode-map
  (when dired-numbering-mode
    (unless (derived-mode-p 'dired-mode 'wdired-mode)
      (setq dired-numbering-mode nil)
      (user-error "Dired-numbering-mode only works in Dired buffers"))
    (dired-numbering--ensure-editable)))

;; The move and renumber commands only ever change a numeric prefix.  This
;; check makes that a guarantee rather than an intention: whatever went wrong
;; in the buffer, a rename that would give a file another file's name never
;; reaches the disk.
(defun dired-numbering--foreign-renames ()
  "Return the pending renames (OLD . NEW) that would alter a base name."
  (let ((renames nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((old (and (dired-numbering--file-on-line) (wdired-get-filename nil t)))
              (new (and (dired-numbering--file-on-line) (wdired-get-filename))))
          (when (and old new
                     (not (equal (dired-numbering--bare (file-name-nondirectory old))
                                 (dired-numbering--bare (file-name-nondirectory new)))))
            (push (cons old new) renames)))
        (forward-line 1)))
    (nreverse renames)))

(defun dired-numbering--guard-finish-edit (&rest _)
  "Refuse to commit renames that change a base name while the mode is on."
  (when dired-numbering-mode
    (let ((foreign (dired-numbering--foreign-renames)))
      (when foreign
        (user-error "Refusing to rename %s to %s: only the number may change (disable `dired-numbering-mode' to edit names)"
                    (caar foreign) (cdar foreign))))))

(advice-add 'wdired-finish-edit :before #'dired-numbering--guard-finish-edit)

(defun dired-numbering-unload-function ()
  "Remove the commit guard when the package is unloaded."
  (advice-remove 'wdired-finish-edit #'dired-numbering--guard-finish-edit)
  nil)

(provide 'dired-numbering)
;;; dired-numbering.el ends here
