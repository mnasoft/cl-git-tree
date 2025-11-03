;;;; ./src/commands/add.lisp

(defpackage :cl-git-tree/commands/add
  (:use :cl)
  (:export cmd-add
           add-repo))

(in-package :cl-git-tree/commands/add)

(defparameter *tracked-patterns*
  '("*.lisp" "*.org" "*.asd" "*.c*" "*.h*" "*.tcl*" ".gitignore")
  "Список шаблонов файлов, которые автоматически добавляются в индекс.")

(defun find-tracked-files (repo-dir &optional (patterns *tracked-patterns*))
  "Ищет файлы по шаблонам PATTERNS внутри REPO-DIR, исключая .git."
  (let* ((args (append
                '("find" "." "-type" "f"
                  "(" "-path" "./.git" "-prune" "-o")
                (reduce (lambda (acc pat)
                          (append acc (list "-name" pat "-o")))
                        (butlast patterns)
                        :initial-value '())
                (list "-name" (car (last patterns)) ")")))
         (cwd (uiop:with-current-directory (repo-dir)
                (nth-value 0
                           (uiop:run-program args
                                             :output :string
                                             :error-output :string
                                             :ignore-error-status t)))))
    (remove-if #'uiop:emptyp
               (uiop:split-string cwd :separator '(#\Newline)))))

(defun add-repo (repo-dir args)
  "Добавляет отслеживаемые файлы в git-индекс."
  (declare (ignore args))
  (let ((files (find-tracked-files repo-dir)))
    (format t "✔ ~A: найдено ~D файл(ов)~%" repo-dir (length files))
    (multiple-value-bind (_out err code)
        (apply #'cl-git-tree/git-utils:git-run repo-dir "add" files)
      (declare (ignore _out))
      (if (zerop code)
          (format t "✔ ~A: файлы добавлены~%" repo-dir)
          (format t "❌ ~A: ошибка при git add:~%~A" repo-dir err)))))

(defun cmd-add (&rest args)
  "CLI-команда: добавить отслеживаемые файлы во все git-репозитории."
  (cond
    ((member "--help" args :test #'string=)
     (format t "Добавляет все отслеживаемые изменения во всех git-репозиториях.~%~%")
     (format t "Использование:~%  git-tree add~%")
     (format t "Пример:~%  git-tree add~%"))
    (t
     (cl-git-tree/fs:with-repo #'add-repo args))))


;; регистрация команды при загрузке
(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "add" #'cmd-add "Добавить файлы в индекс"))
