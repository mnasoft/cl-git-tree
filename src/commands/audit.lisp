;;;; ./src/commands/audit.lisp

(defpackage :cl-git-tree/commands/audit
  (:use :cl)
  (:export cmd-audit))

(in-package :cl-git-tree/commands/audit)

(defun repo-has-unstaged-p (repo-dir)
  "Проверяет, есть ли неиндексированные изменения в репозитории."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run repo-dir "diff" "--name-only")
    (declare (ignore err))
    (and (zerop code) (> (length (string-trim '(#\Space #\Newline #\Return) out)) 0))))

(defun repo-unstaged-files (repo-dir)
  "Возвращает список неиндексированных файлов."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run repo-dir "diff" "--name-only")
    (declare (ignore err))
    (when (zerop code)
      (let ((files (string-trim '(#\Space #\Newline #\Return) out)))
        (when (> (length files) 0)
          (uiop:split-string files :separator (string #\Newline)))))))

(defun repo-has-untracked-p (repo-dir)
  "Проверяет, есть ли неотслеживаемые файлы в репозитории."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run repo-dir "ls-files" "--others" "--exclude-standard")
    (declare (ignore err))
    (and (zerop code) (> (length (string-trim '(#\Space #\Newline #\Return) out)) 0))))

(defun repo-untracked-files (repo-dir)
  "Возвращает список неотслеживаемых файлов."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run repo-dir "ls-files" "--others" "--exclude-standard")
    (declare (ignore err))
    (when (zerop code)
      (let ((files (string-trim '(#\Space #\Newline #\Return) out)))
        (when (> (length files) 0)
          (uiop:split-string files :separator (string #\Newline)))))))

(defun repo-is-clean-p (repo-dir)
  "Проверяет, что репозиторий чист (нет незакоммиченных изменений)."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run repo-dir "status" "--short")
    (declare (ignore err))
    (and (zerop code) (string= out ""))))

(defun cmd-audit (&rest args)
  "CLI-команда: проверяет состояние репозиториев.
  
  Подкоманды:
    status    - показать git status во всех репозиториях
    dirty     - показать репозитории с незакоммиченными изменениями
    staged    - показать репозитории с неиндексированными изменениями (unstaged)
    untracked - показать репозитории с неотслеживаемыми файлами
    --help    - показать эту справку"
  (cond
    ((member "--help" args :test #'string=)
     (format t "Проверяет состояние репозиториев на дереве.~%~%")
     (format t "Использование:~%")
     (format t "  git-tree audit status~%")
     (format t "  git-tree audit dirty~%")
     (format t "  git-tree audit staged~%")
     (format t "  git-tree audit untracked~%")
     (format t "  git-tree audit --help~%~%")
     
     (format t "Подкоманды:~%~%")

     (format t "  status~%")
     (format t "    Показывает git status во всех git-репозиториях, найденных в дереве.~%")
    (format t "~%")
     
    (format t "  dirty~%")
     (format t "    Показывает репозитории с незакоммиченными изменениями (modified,~%")
     (format t "    added, deleted файлы в рабочей директории).~%")
     (format t "    Проверяет: git status --short~%")
     (format t "    Помогает найти репо, где остались неучтённые изменения.~%~%")
     
     (format t "  staged~%")
     (format t "    Показывает репозитории с неиндексированными (unstaged) изменениями.~%")
     (format t "    Выводит список неиндексированных файлов.~%")
     (format t "    Проверяет: git diff --name-only~%")
     (format t "    Помогает найти файлы, которые были изменены, но не добавлены~%")
     (format t "    в индекс перед коммитом.~%~%")
     
    (format t "  untracked~%")
     (format t "    Показывает репозитории с неотслеживаемыми (untracked) файлами.~%")
     (format t "    Выводит список неотслеживаемых файлов.~%")
     (format t "    Проверяет: git ls-files --others --exclude-standard~%")
     (format t "    Помогает обнаружить артефакты (*.o, *.pyc, node_modules, и т.д.),~%")
     (format t "    которые должны быть в .gitignore.~%~%")
     
     (format t "Примечание:~%")
     (format t "  Каждая подкоманда выводит количество найденных репозиториев~%")
     (format t "  и детальный список файлов для каждого репо.~%~%")
     
    (format t "Примеры:~%")
    (format t "  git-tree audit status             # Показать git status во всех репозиториях~%")
    (format t "  git-tree audit dirty              # Найти репо с незакоммиченными изменениями~%")
    (format t "  git-tree audit staged             # Найти репо с unstaged файлами~%")
    (format t "  git-tree audit untracked          # Найти репо с неотслеживаемыми файлами~%"))

      ((and args (string= (first args) "status"))
    (format t "Показываю git status во всех git-репозиториях (через audit).~%")
    (cl-git-tree/fs:with-repo #'cl-git-tree/commands/status:status-repo args))
    
    ((and args (string= (first args) "dirty"))
     (let ((dirty 0))
       (format t "🔍 Поиск репозиториев с незакоммиченными изменениями...~%~%")
       (dolist (repo-dir (cl-git-tree/fs:find-git-repos))
         (unless (repo-is-clean-p repo-dir)
           (incf dirty)
           (format t "⚠️  ~A (~A)~%"
                   (cl-git-tree/fs:repo-name repo-dir)
                   (namestring repo-dir))))
       (if (zerop dirty)
           (format t "Все репозитории чистые.~%")
           (format t "~%Всего грязных репозиториев: ~A~%" dirty))))
    
    ((and args (string= (first args) "staged"))
     (let ((unstaged 0))
       (format t "🔍 Поиск репозиториев с неиндексированными изменениями...~%~%")
       (dolist (repo-dir (cl-git-tree/fs:find-git-repos))
         (when (repo-has-unstaged-p repo-dir)
           (incf unstaged)
           (format t "⚠️  ~A (~A)~%"
                   (cl-git-tree/fs:repo-name repo-dir)
                   (namestring repo-dir))
           (let ((files (repo-unstaged-files repo-dir)))
             (when files
               (dolist (file files)
                 (format t "     • ~A~%" file))))))
       (if (zerop unstaged)
           (format t "Все репозитории полностью проиндексированы.~%")
           (format t "~%Всего репозиториев с unstaged changes: ~A~%" unstaged))))
    
    ((and args (string= (first args) "untracked"))
     (let ((with-untracked 0))
       (format t "🔍 Поиск репозиториев с неотслеживаемыми файлами...~%~%")
       (dolist (repo-dir (cl-git-tree/fs:find-git-repos))
         (when (repo-has-untracked-p repo-dir)
           (incf with-untracked)
           (format t "⚠️  ~A (~A)~%"
                   (cl-git-tree/fs:repo-name repo-dir)
                   (namestring repo-dir))
           (let ((files (repo-untracked-files repo-dir)))
             (when files
               (dolist (file files)
                 (format t "     • ~A~%" file))))))
       (if (zerop with-untracked)
           (format t "Нет репозиториев с неотслеживаемыми файлами.~%")
           (format t "~%Всего репозиториев с untracked файлами: ~A~%" with-untracked))))
    
    (t
     (format t "Используйте: git-tree audit <status|dirty|staged|untracked> или git-tree audit --help~%"))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "audit" #'cmd-audit "Проверить состояние репозиториев"))
