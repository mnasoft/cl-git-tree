;;;; ./src/commands/aliases.lisp

(defpackage :cl-git-tree/commands/aliases
  (:use :cl)
  (:export cmd-aliases))

(in-package :cl-git-tree/commands/aliases)

(defun git-global-config (&rest opts)
  (apply #'cl-git-tree/git-utils:git-run "." "config" "--global" opts))

(defun cmd-aliases (&rest args)
  "CLI-команда: управлять глобальными git-алиасами.
Если первый аргумент = \"list\" → показать текущие алиасы.
Иначе — установить стандартный набор."
  (cond
    ;; режим просмотра
    ((and args (string= (first args) "list"))
     (multiple-value-bind (out err code)
         (cl-git-tree/git-utils:git-run
          "." "config" "--global" "--get-regexp" "^alias\\.")
       (if (zerop code)
           (format t "~A~%" out)
           (format t "⚠️ Не удалось получить список алиасов: ~A~%" err))))
    ;; режим установки
    (t
     (git-global-config "alias.lg" "log --oneline --graph")
     (git-global-config "alias.co" "checkout")
     (git-global-config "alias.br" "branch")
     (git-global-config "alias.ci" "commit")
     (git-global-config "alias.st" "status")
     (git-global-config "alias.visual" "!gitk")
     (git-global-config "alias.tree" "!git-tree")
     (git-global-config "core.editor" "emacs")
     (format t "✔ Git aliases configured globally~%"))))

;; регистрация команды при загрузке
(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "aliases" #'cmd-aliases "Управление глобальными git-алиасами"))
