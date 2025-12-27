;;; Вывод содержимого README.org в две колонки через print-two-columns

(require 'asdf)
(asdf:load-system :cl-git-tree/utils)

(use-package :cl-git-tree/utils/two-columns)

(defun read-lines (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil)
          while line collect line)))

(let* ((lines (read-lines "README.org")))
  (format t "~%=== README.org в две колонки ===~%~%")
  (print-two-columns lines :column-width 50 :separator "   "))
