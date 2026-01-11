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
     (let ((ws (cl-git-tree/loc:make-workspace ".")))
       (multiple-value-bind (out err code)
           (cl-git-tree/git-utils:git-run
            "." "config" "--global" "--get-regexp" "^alias\\.")
         (if (zerop code)
             (format t "~A~%" out)
             (format t "~A Не удалось получить список алиасов: ~A~%" (cl-git-tree/loc:find-emo ws "warning") err)))))
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
     (let ((ws (cl-git-tree/loc:make-workspace ".")))
       (format t "~A Git aliases configured globally~%" 
               (cl-git-tree/loc:find-emo ws "success"))))))

;; регистрация команды при загрузке
(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "aliases" #'cmd-aliases "Управление глобальными git-алиасами"))
