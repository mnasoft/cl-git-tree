;;;; ./src/commands/add.lisp

(defpackage :cl-git-tree/commands/add
  (:use :cl)
  (:import-from cl-git-tree
                *locations*
                location-url-git
                )
  (:import-from cl-git-tree/fs
                repo-name
                with-each-repo
                with-each-repo-simple
                )
  (:import-from cl-git-tree/git-utils
                git-run
                current-branch
                repo-remotes
                )
  (:export run
           ))


(in-package :cl-git-tree/commands/add)

(defparameter *tracked-patterns*
  '("*.lisp" "*.org" "*.asd" "*.c*" "*.h*" "*.tcl*" ".gitignore"))

(defun find-tracked-files (repo-dir &optional (patterns '("*.lisp" "*.org" "*.asd" "*.c*" "*.h*" "*.tcl*" ".gitignore")))
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


(defun add-repo (repo-dir)
  "Добавляет отслеживаемые файлы в git-индекс."
  (multiple-value-bind (files _err _code)
      (find-tracked-files repo-dir)
    (declare (ignore _err _code))
    (format t "✔ ~A: добавлены ~D файл(ов)~%" repo-dir (length files))
    (multiple-value-bind (_out err code)
        (apply #'git-run repo-dir "add" files)
      (declare (ignore _out))
      (if (zerop code)
          (format t "✔ ~A: файлы добавлены~%" repo-dir)
          (format t "❌ ~A: ошибка при git add:~%~A" repo-dir err)))))


(defun run (&rest _args)
  (with-each-repo-simple #'add-repo))

(push (cons "add" #'run) cl-git-tree:*commands*)

