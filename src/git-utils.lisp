(defpackage :cl-git-tree/git-utils
  (:use :cl)
  (:import-from uiop
                run-program
                split-string
                emptyp)
  (:export git-run
           current-branch
           repo-remotes))

(in-package :cl-git-tree/git-utils)

(defun git-run (repo-dir &rest args)
  "Запускает git с аргументами ARGS в каталоге REPO-DIR.
Возвращает три значения: stdout, stderr, код возврата."
  (uiop:run-program
   (append (list "git" "-C" (namestring repo-dir)) args)
   :output :string :error-output :string
   :ignore-error-status t))

(defun current-branch (repo-dir)
  "Возвращает имя текущей ветки в репозитории."
  (string-trim
   '(#\Newline #\Return)
   (nth-value 0 (git-run repo-dir "rev-parse" "--abbrev-ref" "HEAD"))))

(defun repo-remotes (repo-dir)
  "Возвращает список всех remotes в репозитории."
  (remove-if #'emptyp
             (split-string
              (nth-value 0 (git-run repo-dir "remote"))
              :separator '(#\Newline))))
