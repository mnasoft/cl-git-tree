;;;; ./src/commands/aliases.lisp
(defpackage :cl-git-tree/commands/aliases
  (:use :cl)
  (:import-from cl-git-tree/git-utils
                git-run)
  (:export run))

(in-package :cl-git-tree/commands/aliases)

(defun git-global-config (&rest opts)
  (apply #'git-run "." "config" "--global" opts))

(defun run (&rest args)
  "Если аргумент = list → показать текущие алиасы.
   Иначе — установить стандартный набор."
  (cond
    ;; режим просмотра
    ((and args (string= (first args) "list"))
     (multiple-value-bind (out err code)
         (git-run "." "config" "--global" "--get-regexp" "^alias\\.")
       (if (zerop code)
           (format t "~A~%" out)
           (format t "⚠ Не удалось получить список алиасов: ~A~%" err))))
    ;; режим установки
    (t
     (git-global-config "alias.lg" "log --oneline --graph")
     (git-global-config "alias.co" "checkout")
     (git-global-config "alias.br" "branch")
     (git-global-config "alias.ci" "commit")
     (git-global-config "alias.st" "status")
     (git-global-config "alias.visual" "!gitk")
     (git-global-config "alias.tree" "!git-tree.lisp")
     (git-global-config "core.editor" "emacs")
     (format t "✔ Git aliases configured globally~%"))))

(push (cons "aliases" #'run) cl-git-tree:*commands*)
