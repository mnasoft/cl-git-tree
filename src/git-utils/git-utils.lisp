;;;; ./src/git-utils/git-utils.lisp

(in-package :cl-git-tree/git-utils)

(defun git-run (repo-dir &rest args)
  "Запускает команду git с аргументами ARGS в каталоге REPO-DIR.
Возвращает три значения:
  1. stdout (строка),
  2. stderr (строка),
  3. код возврата (целое число)."
  (uiop:run-program
   (append (list "git" "-C" (namestring repo-dir)) args)
   :output :string
   :error-output :string
   :ignore-error-status t))

(defun current-branch (repo-dir)
  "Возвращает имя текущей ветки в репозитории REPO-DIR как строку.
Если HEAD отсоединён, git вернёт 'HEAD'."
  (string-trim
   '(#\Newline #\Return)
   (nth-value 0 (git-run repo-dir "rev-parse" "--abbrev-ref" "HEAD"))))

(defun repo-remotes (repo-dir)
  "Возвращает список имён всех remotes в репозитории REPO-DIR.
Результат — список строк (например, '(\"origin\" \"upstream\"))."
  (remove-if #'uiop:emptyp
             (uiop:split-string
              (nth-value 0 (git-run repo-dir "remote"))
              :separator '(#\Newline))))
