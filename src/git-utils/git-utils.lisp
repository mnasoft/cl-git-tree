;;;; ./src/git-utils/git-utils.lisp

(in-package :cl-git-tree/git-utils)

(defun normalize-path-for-git (path)
  "Преобразует путь для совместимости с git под MSYS2.
На MSYS2 конвертирует POSIX-пути в Windows-формат используя cygpath.
На других платформах возвращает путь как есть."
  (let* ((path-str (namestring path))
         ;; Раскрываем ~/ или ~ в начале
         (expanded (if (and (> (length path-str) 0) (char= (aref path-str 0) #\~))
                       ;; Используем shell для раскрытия ~
                       (let ((out (uiop:run-program
                                   (list "sh" "-c" (format nil "echo ~A" path-str))
                                   :output :string :error-output :string :ignore-error-status t)))
                         (string-trim '(#\Newline #\Return) out))
                       path-str)))
    (handler-case
        (let ((result (uiop:run-program
                       (list "uname" "-o")
                       :output :string
                       :error-output :string
                       :ignore-error-status t)))
          (if (and result (string= "Msys" (string-trim '(#\Newline) result)))
              ;; MSYS2: конвертируем в Windows-путь
              (let ((converted (uiop:run-program
                                (list "cygpath" "-m" expanded)
                                :output :string
                                :error-output :string
                                :ignore-error-status t)))
                (if converted
                    (string-trim '(#\Newline) converted)
                    expanded))
              ;; Другие платформы: возвращаем как есть
              expanded))
      (error (c)
        ;; Если ошибка при определении OS, возвращаем раскрытый путь
        (declare (ignore c))
        expanded))))

(defun git-run (repo-dir &rest args)
  "Запускает команду git с аргументами ARGS в каталоге REPO-DIR.
Возвращает три значения:
  1. stdout (строка),
  2. stderr (строка),
  3. код возврата (целое число)."
  (uiop:run-program
   (append (list "git" "-C" (normalize-path-for-git repo-dir)) args)
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
