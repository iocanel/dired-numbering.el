;;; test-dired-numbering.el --- Test dired-numbering -*- lexical-binding: t -*-

;; Author: Ioannis Canellos

;;; Commentary:
;;
;; Every test builds a real directory, drives the mode, commits with wdired
;; and then checks the disk, not just the buffer.
;;
;; Each file holds its own base name (the name without the numeric prefix) as
;; content.  After every commit the suite asserts that each file on disk still
;; holds the base name its filename shows.  That is the property that matters
;; for a CD: the audio of a track must never end up under another title.
;;
;; The tests go from the simplest cases to the most complex ones:
;;
;;   01-09  one and two files
;;   10-19  three files, every move up and down
;;   20-29  longer sequences and round trips
;;   30-39  commits in between moves
;;   40-49  unnumbered, gapped and badly padded directories
;;   50-59  names that are easy to get wrong
;;   60-69  the actual key bindings
;;   70-79  the commit guard
;;   90-99  randomized sequences checked against a model

;;; Code:

(require 'ert)
(require 'seq)
(load (expand-file-name "../dired-numbering.el" (file-name-directory load-file-name)))
(require 'dired-numbering)

;;; Helpers

(defmacro dired-numbering-test-with-dir (names &rest body)
  "Create a directory holding NAMES and run BODY in its dired buffer.
Each file contains its own base name.  The mode is enabled."
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "dired-numbering" t)))
          (dired-listing-switches "-al"))
     (unwind-protect
         (progn
           (dolist (name ,names)
             (with-temp-file (expand-file-name name dir)
               (insert (dired-numbering--bare name))))
           (when (get-buffer "*Dired log*") (kill-buffer "*Dired log*"))
           (let ((buffer (dired-noselect dir)))
             (unwind-protect
                 (with-current-buffer buffer
                   (switch-to-buffer buffer)
                   (dired-numbering-mode 1)
                   ,@body)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))
       (delete-directory dir t))))

(defun dired-numbering-test--disk ()
  "Files on disk, sorted, as (NAME . CONTENT).
A directory stands for its own base name, it has no content to compare."
  (mapcar (lambda (name)
            (let ((file (expand-file-name name default-directory)))
              (cons name (if (file-directory-p file)
                             (dired-numbering--bare name)
                           (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))))
          (directory-files default-directory nil "\\`[^.]")))

(defun dired-numbering-test--disk-names ()
  "Filenames on disk, sorted."
  (mapcar #'car (dired-numbering-test--disk)))

(defun dired-numbering-test--mismatches ()
  "Files whose content is not the base name their filename shows."
  (seq-remove (lambda (entry) (equal (dired-numbering--bare (car entry)) (cdr entry)))
              (dired-numbering-test--disk)))

(defun dired-numbering-test--dired-log ()
  "Content of the dired log, where wdired reports failed renames."
  (let ((log (get-buffer "*Dired log*")))
    (if log (with-current-buffer log (buffer-string)) "")))

(defun dired-numbering-test--commit ()
  "Commit the pending renames and assert that nothing went wrong."
  (let ((inhibit-message t))
    (wdired-finish-edit))
  (should (equal (dired-numbering-test--dired-log) ""))
  (should (equal (dired-numbering-test--mismatches) nil))
  (should (equal (dired-numbering--file-names) (dired-numbering-test--disk-names))))

(defun dired-numbering-test--move (name direction &optional times)
  "Move the file NAME one position in DIRECTION (`up' or `down'), TIMES times."
  (should (dired-numbering--goto-file name))
  (let ((inhibit-message t))
    (dotimes (_ (or times 1))
      (if (eq direction 'up)
          (dired-numbering-move-up)
        (dired-numbering-move-down)))))

(defun dired-numbering-test--expect (names)
  "Assert that the buffer shows NAMES, then commit and assert the disk does."
  (should (equal (dired-numbering--file-names) names))
  (dired-numbering-test--commit)
  (should (equal (dired-numbering-test--disk-names) names)))

(defun dired-numbering-test--tracks (&rest bares)
  "Number BARES 01, 02, ... in order."
  (let ((index 0))
    (mapcar (lambda (bare) (setq index (1+ index)) (format "%02d - %s" index bare))
            bares)))

;;; 01-09: one and two files

(ert-deftest dired-numbering-test-01-enable-renames-nothing ()
  (dired-numbering-test-with-dir '("b.mp3" "a.mp3")
    (should (derived-mode-p 'wdired-mode))
    (dired-numbering-test--expect '("a.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-02-single-file-up-and-down ()
  (dired-numbering-test-with-dir '("01 - a.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'up)
    (dired-numbering-test--move "01 - a.mp3" 'down)
    (dired-numbering-test--expect '("01 - a.mp3"))))

(ert-deftest dired-numbering-test-03-two-files-down ()
  (dired-numbering-test-with-dir '("01 - a.mp3" "02 - b.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down)
    (should (equal (dired-numbering--file-on-line) "02 - a.mp3"))
    (dired-numbering-test--expect (dired-numbering-test--tracks "b.mp3" "a.mp3"))))

(ert-deftest dired-numbering-test-04-two-files-up ()
  (dired-numbering-test-with-dir '("01 - a.mp3" "02 - b.mp3")
    (dired-numbering-test--move "02 - b.mp3" 'up)
    (should (equal (dired-numbering--file-on-line) "01 - b.mp3"))
    (dired-numbering-test--expect (dired-numbering-test--tracks "b.mp3" "a.mp3"))))

(ert-deftest dired-numbering-test-05-two-files-down-then-up ()
  (dired-numbering-test-with-dir '("01 - a.mp3" "02 - b.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down)
    (dired-numbering-test--move "02 - a.mp3" 'up)
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-06-two-files-edges-are-noops ()
  (dired-numbering-test-with-dir '("01 - a.mp3" "02 - b.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'up)
    (dired-numbering-test--move "02 - b.mp3" 'down)
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-07-two-files-swap-twice ()
  (dired-numbering-test-with-dir '("01 - a.mp3" "02 - b.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down)
    (dired-numbering-test--move "01 - b.mp3" 'down)
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "b.mp3"))))

;;; 10-19: three files

(ert-deftest dired-numbering-test-10-first-to-last ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down 2)
    (should (equal (dired-numbering--file-on-line) "03 - a.mp3"))
    (dired-numbering-test--expect (dired-numbering-test--tracks "b.mp3" "c.mp3" "a.mp3"))))

(ert-deftest dired-numbering-test-11-last-to-first ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering-test--move "03 - c.mp3" 'up 2)
    (should (equal (dired-numbering--file-on-line) "01 - c.mp3"))
    (dired-numbering-test--expect (dired-numbering-test--tracks "c.mp3" "a.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-12-middle-up ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering-test--move "02 - b.mp3" 'up)
    (dired-numbering-test--expect (dired-numbering-test--tracks "b.mp3" "a.mp3" "c.mp3"))))

(ert-deftest dired-numbering-test-13-middle-down ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering-test--move "02 - b.mp3" 'down)
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "c.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-14-past-the-bottom-stops ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down 5)
    (dired-numbering-test--expect (dired-numbering-test--tracks "b.mp3" "c.mp3" "a.mp3"))))

(ert-deftest dired-numbering-test-15-past-the-top-stops ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering-test--move "03 - c.mp3" 'up 5)
    (dired-numbering-test--expect (dired-numbering-test--tracks "c.mp3" "a.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-16-down-then-up-the-same-file ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down 2)
    (dired-numbering-test--move "03 - a.mp3" 'up 1)
    (dired-numbering-test--expect (dired-numbering-test--tracks "b.mp3" "a.mp3" "c.mp3"))))

(ert-deftest dired-numbering-test-17-two-files-each-move ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down 2)
    (dired-numbering-test--move "01 - b.mp3" 'down)
    (dired-numbering-test--expect (dired-numbering-test--tracks "c.mp3" "b.mp3" "a.mp3"))))

;;; 20-29: longer sequences and round trips

(ert-deftest dired-numbering-test-20-round-trip-down-then-up ()
  (let ((tracks (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3" "d.mp3" "e.mp3")))
    (dired-numbering-test-with-dir tracks
      (dired-numbering-test--move "01 - a.mp3" 'down 4)
      (dired-numbering-test--move "05 - a.mp3" 'up 4)
      (dired-numbering-test--expect tracks))))

(ert-deftest dired-numbering-test-21-round-trip-up-then-down ()
  (let ((tracks (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3" "d.mp3" "e.mp3")))
    (dired-numbering-test-with-dir tracks
      (dired-numbering-test--move "04 - d.mp3" 'up 3)
      (dired-numbering-test--move "01 - d.mp3" 'down 3)
      (dired-numbering-test--expect tracks))))

(ert-deftest dired-numbering-test-22-reverse-the-whole-list ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3" "d.mp3" "e.mp3")
    ;; Carry what is at the top down to its final place, one file at a time.
    (dired-numbering-test--move "01 - a.mp3" 'down 4)
    (dired-numbering-test--move "01 - b.mp3" 'down 3)
    (dired-numbering-test--move "01 - c.mp3" 'down 2)
    (dired-numbering-test--move "01 - d.mp3" 'down 1)
    (dired-numbering-test--expect
     (dired-numbering-test--tracks "e.mp3" "d.mp3" "c.mp3" "b.mp3" "a.mp3"))))

(ert-deftest dired-numbering-test-23-reverse-moving-up ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3" "d.mp3" "e.mp3")
    (dired-numbering-test--move "05 - e.mp3" 'up 4)
    (dired-numbering-test--move "05 - d.mp3" 'up 3)
    (dired-numbering-test--move "05 - c.mp3" 'up 2)
    (dired-numbering-test--move "05 - b.mp3" 'up 1)
    (dired-numbering-test--expect
     (dired-numbering-test--tracks "e.mp3" "d.mp3" "c.mp3" "b.mp3" "a.mp3"))))

(ert-deftest dired-numbering-test-24-both-ways-interleaved ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3" "d.mp3" "e.mp3")
    (dired-numbering-test--move "02 - b.mp3" 'down 2)   ; a c d b e
    (dired-numbering-test--move "05 - e.mp3" 'up 3)     ; a e c d b
    (dired-numbering-test--move "01 - a.mp3" 'down 1)   ; e a c d b
    (dired-numbering-test--move "05 - b.mp3" 'up 4)     ; b e a c d
    (dired-numbering-test--expect
     (dired-numbering-test--tracks "b.mp3" "e.mp3" "a.mp3" "c.mp3" "d.mp3"))))

(ert-deftest dired-numbering-test-25-across-the-ten-boundary ()
  (dired-numbering-test-with-dir (apply #'dired-numbering-test--tracks
                                        (mapcar (lambda (i) (format "t%d.mp3" i))
                                                (number-sequence 1 12)))
    (dired-numbering-test--move "12 - t12.mp3" 'up 11)
    (dired-numbering-test--move "02 - t1.mp3" 'down 10)
    (dired-numbering-test--expect
     (apply #'dired-numbering-test--tracks
            (append '("t12.mp3") (mapcar (lambda (i) (format "t%d.mp3" i)) (number-sequence 2 11))
                    '("t1.mp3"))))))

;;; 30-39: commits in between moves

(ert-deftest dired-numbering-test-30-move-after-commit ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down)
    (dired-numbering-test--commit)
    (should-not (derived-mode-p 'wdired-mode))
    (dired-numbering-test--move "01 - b.mp3" 'down)
    (should (derived-mode-p 'wdired-mode))
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-31-commit-after-every-move ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3" "d.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down)
    (dired-numbering-test--commit)
    (dired-numbering-test--move "02 - a.mp3" 'down)
    (dired-numbering-test--commit)
    (dired-numbering-test--move "03 - a.mp3" 'down)
    (dired-numbering-test--commit)
    (dired-numbering-test--move "04 - a.mp3" 'up 3)
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3" "d.mp3"))))

(ert-deftest dired-numbering-test-32-move-after-abort ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down 2)
    (let ((inhibit-message t)) (wdired-abort-changes))
    (should (equal (dired-numbering-test--disk-names) (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")))
    (dired-numbering-test--move "03 - c.mp3" 'up)
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "c.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-33-commit-without-changes ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3")
    (dired-numbering-test--move "01 - a.mp3" 'down)
    (dired-numbering-test--move "02 - a.mp3" 'up)
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "b.mp3"))))

;;; 40-49: unnumbered, gapped and badly padded directories

(ert-deftest dired-numbering-test-40-move-in-unnumbered-dir-numbers-first ()
  (dired-numbering-test-with-dir '("x.mp3" "y.mp3" "z.mp3")
    (dired-numbering-test--move "z.mp3" 'up)
    (should (equal (dired-numbering--file-on-line) "02 - z.mp3"))
    (dired-numbering-test--expect (dired-numbering-test--tracks "x.mp3" "z.mp3" "y.mp3"))))

(ert-deftest dired-numbering-test-41-edge-move-in-unnumbered-dir-still-numbers ()
  (dired-numbering-test-with-dir '("x.mp3" "y.mp3")
    (dired-numbering-test--move "x.mp3" 'up)
    (dired-numbering-test--expect (dired-numbering-test--tracks "x.mp3" "y.mp3"))))

(ert-deftest dired-numbering-test-42-renumber-closes-gaps ()
  (dired-numbering-test-with-dir '("01 - a.mp3" "02 - b.mp3" "05 - c.mp3" "09 - d.mp3")
    (let ((inhibit-message t)) (dired-numbering-renumber))
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3" "d.mp3"))))

(ert-deftest dired-numbering-test-43-move-with-gaps-keeps-the-gap-order ()
  (dired-numbering-test-with-dir '("01 - a.mp3" "05 - b.mp3" "09 - c.mp3")
    (dired-numbering-test--move "09 - c.mp3" 'up)
    (dired-numbering-test--expect '("01 - a.mp3" "05 - c.mp3" "09 - b.mp3"))))

(ert-deftest dired-numbering-test-44-unpadded-numbers-sort-numerically ()
  ;; ls lists "10 - j" before "2 - b"; the numbers say otherwise.
  (dired-numbering-test-with-dir '("1 - a.mp3" "2 - b.mp3" "10 - j.mp3")
    (dired-numbering-test--move "2 - b.mp3" 'down)
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "j.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-45-mixed-numbered-and-unnumbered ()
  (dired-numbering-test-with-dir '("02 - b.mp3" "01 - a.mp3" "c.mp3")
    (dired-numbering-test--move "c.mp3" 'up)
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "c.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-46-duplicate-numbers ()
  (dired-numbering-test-with-dir '("01 - a.mp3" "01 - b.mp3" "02 - c.mp3")
    (dired-numbering-test--move "02 - c.mp3" 'up)
    (should (dired-numbering--in-order-p))
    (dired-numbering-test--commit)
    (should (equal (length (dired-numbering-test--disk-names)) 3))))

(ert-deftest dired-numbering-test-47-padding-grows-past-ninety-nine ()
  (dired-numbering-test-with-dir (mapcar (lambda (i) (format "t%03d.mp3" i)) (number-sequence 1 100))
    (let ((inhibit-message t)) (dired-numbering-renumber))
    (should (equal (car (dired-numbering--file-names)) "001 - t001.mp3"))
    (should (equal (car (last (dired-numbering--file-names))) "100 - t100.mp3"))
    (dired-numbering-test--commit)))

;;; 50-59: names that are easy to get wrong

(ert-deftest dired-numbering-test-50-greek-names ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "Άλφα.mp3" "Βήτα γάμμα.mp3" "Δέλτα.mp3")
    (dired-numbering-test--move "01 - Άλφα.mp3" 'down 2)
    (dired-numbering-test--move "01 - Βήτα γάμμα.mp3" 'down)
    (dired-numbering-test--expect
     (dired-numbering-test--tracks "Δέλτα.mp3" "Βήτα γάμμα.mp3" "Άλφα.mp3"))))

(ert-deftest dired-numbering-test-51-names-with-the-separator-inside ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "Artist - One.mp3" "Artist - Two.mp3")
    (dired-numbering-test--move "01 - Artist - One.mp3" 'down)
    (dired-numbering-test--expect
     (dired-numbering-test--tracks "Artist - Two.mp3" "Artist - One.mp3"))))

(ert-deftest dired-numbering-test-52-names-that-are-substrings-of-each-other ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a b c.mp3" "a b.mp3" "b.mp3" "ab.mp3")
    (dired-numbering-test--move "04 - ab.mp3" 'up 3)
    (dired-numbering-test--move "02 - a b c.mp3" 'down 2)
    (dired-numbering-test--expect
     (dired-numbering-test--tracks "ab.mp3" "a b.mp3" "b.mp3" "a b c.mp3"))))

(ert-deftest dired-numbering-test-53-sidecar-files-with-the-same-stem ()
  (dired-numbering-test-with-dir '("01 - Song.mp3" "02 - Song.txt" "03 - Other.mp3" "04 - Other.txt")
    (dired-numbering-test--move "03 - Other.mp3" 'up 2)
    (dired-numbering-test--move "04 - Other.txt" 'up 2)
    (dired-numbering-test--expect '("01 - Other.mp3" "02 - Other.txt" "03 - Song.mp3" "04 - Song.txt"))))

(ert-deftest dired-numbering-test-54-names-with-special-characters ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "it's (live) [2001].mp3" "a&b #1, 50%.mp3")
    (dired-numbering-test--move "01 - it's (live) [2001].mp3" 'down)
    (dired-numbering-test--expect
     (dired-numbering-test--tracks "a&b #1, 50%.mp3" "it's (live) [2001].mp3"))))

(ert-deftest dired-numbering-test-56-subdirectory-moves-with-its-name ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3")
    (make-directory (expand-file-name "03 - extras"))
    (revert-buffer)
    (dired-numbering-mode 1)
    (dired-numbering-test--move "03 - extras" 'up 2)
    (dired-numbering-test--commit)
    (should (file-directory-p (expand-file-name "01 - extras")))
    (should (equal (dired-numbering-test--disk-names)
                   '("01 - extras" "02 - a.mp3" "03 - b.mp3")))))

;;; 60-69: the actual key bindings

(defun dired-numbering-test--press (keys &optional times)
  "Press KEYS, TIMES times, through the command loop."
  (let ((inhibit-message t))
    (dotimes (_ (or times 1))
      (execute-kbd-macro (kbd keys)))))

(ert-deftest dired-numbering-test-60-keys-move-down-and-up ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering--goto-file "01 - a.mp3")
    (dired-numbering-test--press "M-<down>" 2)
    (dired-numbering-test--press "M-<up>")
    (dired-numbering-test--expect (dired-numbering-test--tracks "b.mp3" "a.mp3" "c.mp3"))))

(ert-deftest dired-numbering-test-61-keys-commit-and-continue ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3")
    (dired-numbering--goto-file "03 - c.mp3")
    (dired-numbering-test--press "M-<up>" 2)
    (dired-numbering-test--press "C-c C-c")
    (should (equal (dired-numbering-test--mismatches) nil))
    (dired-numbering--goto-file "01 - c.mp3")
    (dired-numbering-test--press "M-<down>")
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "c.mp3" "b.mp3"))))

(ert-deftest dired-numbering-test-62-keys-renumber ()
  (dired-numbering-test-with-dir '("01 - a.mp3" "04 - b.mp3" "07 - c.mp3")
    (dired-numbering-test--press "C-c C-r")
    (dired-numbering-test--expect (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3"))))

;;; 70-79: the commit guard

(ert-deftest dired-numbering-test-70-guard-refuses-a-changed-base-name ()
  (dired-numbering-test-with-dir (dired-numbering-test--tracks "a.mp3" "b.mp3")
    (dired-numbering--goto-file "01 - a.mp3")
    (dired-numbering--set-filename "01 - b.mp3.bak")
    (should-error (wdired-finish-edit) :type 'user-error)
    (should (equal (dired-numbering-test--disk-names) (dired-numbering-test--tracks "a.mp3" "b.mp3")))))

(ert-deftest dired-numbering-test-71-guard-allows-name-edits-without-the-mode ()
  (dired-numbering-test-with-dir '("a.mp3")
    (dired-numbering-mode -1)
    (dired-numbering--goto-file "a.mp3")
    (dired-numbering--set-filename "b.mp3")
    (let ((inhibit-message t)) (wdired-finish-edit))
    (should (equal (dired-numbering-test--disk-names) '("b.mp3")))))

;;; 90-99: randomized sequences checked against a model

(defun dired-numbering-test--model-names (model)
  "The filenames MODEL, a list of base names in order, should produce."
  (let ((fmt (format "%%0%dd - %%s" (dired-numbering--width (length model))))
        (index 0))
    (mapcar (lambda (bare) (setq index (1+ index)) (format fmt index bare)) model)))

(defun dired-numbering-test--model-move (model position direction)
  "Return MODEL after moving the base name at POSITION in DIRECTION."
  (let ((other (if (eq direction 'up) (1- position) (1+ position))))
    (if (or (< other 0) (>= other (length model)))
        model
      (let ((copy (copy-sequence model)))
        (setf (nth position copy) (nth other model)
              (nth other copy) (nth position model))
        copy))))

(defun dired-numbering-test--initial-model ()
  "Base names in the order the mode will number them.
Numbered files come first by number, unnumbered ones after in buffer order."
  (mapcar #'dired-numbering--bare
          (sort (dired-numbering--file-names)
                (lambda (x y)
                  (< (or (dired-numbering--number x) most-positive-fixnum)
                     (or (dired-numbering--number y) most-positive-fixnum))))))

(defun dired-numbering-test--goto-bare (bare)
  "Put point on the file whose base name is BARE."
  (should (dired-numbering--goto-file
           (seq-find (lambda (name) (equal (dired-numbering--bare name) bare))
                     (dired-numbering--file-names)))))

(defun dired-numbering-test--random-run (seed names steps)
  "Apply STEPS random operations to a directory of NAMES, seeded with SEED.
After every operation the buffer must match a simple list model, and after
every commit so must the disk."
  (random (format "dired-numbering-%s" seed))
  (dired-numbering-test-with-dir names
    (let ((model (dired-numbering-test--initial-model)))
      (dotimes (_ steps)
        (let* ((position (random (length model)))
               (roll (random 20)))
          (cond
           ((< roll 9)
            (dired-numbering-test--goto-bare (nth position model))
            (let ((inhibit-message t)) (dired-numbering-move-up))
            (setq model (dired-numbering-test--model-move model position 'up)))
           ((< roll 18)
            (dired-numbering-test--goto-bare (nth position model))
            (let ((inhibit-message t)) (dired-numbering-move-down))
            (setq model (dired-numbering-test--model-move model position 'down)))
           ((< roll 19)
            (let ((inhibit-message t)) (dired-numbering-renumber)))
           ((and (derived-mode-p 'wdired-mode) (dired-numbering--in-order-p))
            (dired-numbering-test--commit)
            (should (equal (dired-numbering-test--disk-names)
                           (dired-numbering-test--model-names model)))))
          (when (dired-numbering--in-order-p)
            (should (equal (dired-numbering--file-names)
                           (dired-numbering-test--model-names model))))))
      (let ((inhibit-message t)) (dired-numbering-renumber))
      (dired-numbering-test--expect (dired-numbering-test--model-names model)))))

(ert-deftest dired-numbering-test-90-random-numbered ()
  (dotimes (seed 60)
    (dired-numbering-test--random-run
     seed (dired-numbering-test--tracks "a.mp3" "b.mp3" "c.mp3" "d.mp3" "e.mp3" "f.mp3") 40)))

(ert-deftest dired-numbering-test-91-random-unnumbered ()
  (dotimes (seed 40)
    (dired-numbering-test--random-run seed '("u.mp3" "v.mp3" "w.mp3" "x.mp3" "y.mp3") 40)))

(ert-deftest dired-numbering-test-92-random-greek-with-sidecars ()
  (dotimes (seed 40)
    (dired-numbering-test--random-run
     seed (dired-numbering-test--tracks "Άλφα.mp3" "Άλφα.txt" "Βήτα γάμμα.mp3" "Βήτα γάμμα.txt"
                                        "Artist - Δέλτα.mp3" "cover.jpg")
     40)))

(ert-deftest dired-numbering-test-93-random-across-the-ten-boundary ()
  (dotimes (seed 20)
    (dired-numbering-test--random-run
     seed (mapcar (lambda (i) (format "%d - t%d.mp3" i i)) (number-sequence 1 12)) 60)))

(provide 'test-dired-numbering)
;;; test-dired-numbering.el ends here
