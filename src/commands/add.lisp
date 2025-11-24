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
  "Добавляет отслеживаемые файлы в git-индекс через метод repo-add."
  (let* ((wk (make-instance 'cl-git-tree/loc:<workspace> :path repo-dir))
         (alist    (cl-git-tree/shell-utils:split-args-by-keys args))
         (patterns (or (cdr (assoc :ARGS alist))
                       (cdr (assoc :PREAMBLE alist))))
         (dirs     (cdr (assoc :EXCLUDE alist))))
    ;; если паттерны не заданы явно, используем дефолтные
    (unless patterns (setf patterns *tracked-patterns*))
    ;; если исключения не заданы явно, используем дефолтные
    (unless dirs     (setf dirs *excludes-patterns*))
    ;; ищем файлы
    (let ((files (find-tracked-files (cl-git-tree/loc:git-root wk)
                                     :patterns patterns
                                     :excludes dirs)))
      (format t "✔ ~A: найдено ~D файл(ов)~%" repo-dir (length files))
      ;; вызываем метод repo-add
      (cl-git-tree/loc:repo-add wk :files files))))

(defun cmd-add (&rest args)
  "CLI-команда: добавить отслеживаемые файлы во все git-репозитории."
  (let ((alist (cl-git-tree/shell-utils:split-args-by-keys args)))
    (cond
      ;; если указан ключ --help
      ((assoc :HELP alist)
       (format t "Добавляет все отслеживаемые изменения во всех git-репозиториях.~%~%")
       (format t "Использование:~%")
       (format t "  git-tree add [--exclude DIR ...] -- [PATTERNS...]~%")
       (format t "  git-tree add [PATTERNS...]~%~%")
       (format t "Аргументы:~%")
       (format t "  :EXCLUDE  → каталоги для исключения.~%")
       (format t "              По умолчанию: ~{~A ~}~%" *excludes-patterns*)
       (format t "  :ARGS     → шаблоны файлов для включения.~%")
       (format t "              По умолчанию: ~{~A ~}~%" *tracked-patterns*)
       (format t "~%Примеры:~%")
       (format t "  git-tree add --exclude ./tests ./build -- *.lisp *.asd~%")
       (format t "  git-tree add *.lisp *.asd~%")
       (format t "  git-tree add --   ;; использовать значения по умолчанию~%"))
      ;; иначе запускаем add-repo для каждого репозитория
      (t
       (cl-git-tree/fs:with-repo #'add-repo args)))))

;; регистрация команды при загрузке
(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "add" #'cmd-add "Добавить файлы в индекс"))
