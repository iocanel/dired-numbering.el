;;; run-tests.el --- Run Tests -*- lexical-binding: t -*-

;; Author: Ioannis Canellos

;;; Commentary:

;;; Code:

(defvar root-test-path (file-name-directory (file-truename load-file-name)) "The path where the tests are located.")
(defvar root-code-path (file-name-directory (directory-file-name root-test-path)) "The path where the code is located.")

(add-to-list 'load-path root-code-path)
(load (expand-file-name "test-dired-numbering.el" root-test-path))

(provide 'run-tests)
;;; run-tests.el ends here
