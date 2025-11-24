;;;; ./src/commands/add.lisp

(defpackage :cl-git-tree/commands/add
  (:use :cl)
  (:export cmd-add
           add-repo))

(in-package :cl-git-tree/commands/add)

(defparameter *tracked-patterns*
  '("*.lisp" "*.org" "*.asd" "*.c*" "*.h*" "*.tcl*" ".gitignore")
  "Список шаблонов файлов, которые автоматически добавляются в индекс.")

(defparameter *excludes-patterns*
  '("./.git" "./build")
  "Список шаблонов файлов, которые автоматически добавляются в индекс.")

(defun find-tracked-files (repo-dir
                           &key
                             (patterns *tracked-patterns*)
                             (excludes *excludes-patterns*))
  "Ищет файлы по шаблонам PATTERNS внутри REPO-DIR, исключая .git.
   Команда формируется как строка."
  (let ((cwd
          (uiop:with-current-directory (repo-dir)
            (nth-value
             0
             (uiop:run-program
              (format nil "find . \\( ~{-path ~S ~^-o ~}\\) -prune -o -type f \\(~{ -name ~S ~^-o ~}\\) -print" excludes patterns)
              :output :string
              :error-output :string
              :ignore-error-status t
              :force-shell t)))))
    (remove-if #'uiop:emptyp
               (uiop:split-string cwd :separator '(#\Newline)))))

(defun add-repo (repo-dir args)
  "Добавляет отслеживаемые файлы в git-индекс."   
  (let* ((wk (make-instance 'cl-git-tree/loc:<workspace> :path repo-dir))
         (alist    (cl-git-tree/shell-utils:split-args-by-keys args))
         (patterns (cdr (assoc :PREAMBLE alist)))
         (dirs     (cdr (assoc :EXCLUDE  alist))))
    (unless patterns (setf patterns *tracked-patterns*))
    (unless dirs     (setf dirs     *excludes-patterns*))
    (let ((files (find-tracked-files
             (cl-git-tree/loc:git-root wk)
             :patterns patterns
             :excludes dirs)))
      (format t "✔ ~A: найдено ~D файл(ов)~%" repo-dir (length files))
      (multiple-value-bind (_out err code)
          (apply #'cl-git-tree/git-utils:git-run repo-dir "add" files)
        (declare (ignore _out))
        (if (zerop code)
            (format t "✔ ~A: файлы добавлены~%" repo-dir)
            (format t "❌ ~A: ошибка при git add:~%~A" repo-dir err))))))

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
