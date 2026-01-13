;;;; ./src/commands/single-repo/switch.lisp

(defpackage :cl-git-tree/commands/single-repo/switch
  (:use :cl)
  (:export cmd-switch))

(in-package :cl-git-tree/commands/single-repo/switch)

(defun cmd-switch (&rest args)
  "CLI-команда: переключиться на ветку в текущем репозитории.
Использование:
  git-tree switch [опции] BRANCH"
  (cond
    ((member "--help" args :test #'string=)
     (format t "Переключиться на ветку в текущем git-репозитории (alias для git switch).~%~%")
     (format t "Использование:~%")
     (format t "  git-tree switch [опции] BRANCH~%~%")
     (format t "Опции:~%")
     (format t "  -c, --create           создать и переключиться на новую ветку~%")
     (format t "  -C, --force-create     создать/пересоздать ветку~%")
     (format t "  -d, --detach           отсоединённое состояние HEAD~%")
     (format t "  --orphan               создать ветку-сироту~%")
     (format t "  --no-guess             не угадывать удалённую ветку~%")
     (format t "~%Пример:~%")
     (format t "  git-tree switch main~%")
     (format t "  git-tree switch -c feature/new-feature~%"))
    ((null args)
     (format t "~A Укажите имя ветки~%"
             (cl-git-tree/loc:find-emo (cl-git-tree/loc:make-workspace ".") "error")))
    (t
     (let* ((ws (cl-git-tree/loc:make-workspace "."))
            (branch (first args))
            (opts-args (rest args))
            (create (member "-c" opts-args :test #'string=))
            (create-force (member "-C" opts-args :test #'string=))
            (detach (member "-d" opts-args :test #'string=))
            (detach-long (member "--detach" opts-args :test #'string=))
            (orphan (member "--orphan" opts-args :test #'string=))
            (no-guess (member "--no-guess" opts-args :test #'string=)))
       (handler-case
           (cl-git-tree/loc:repo-switch ws branch
                                        :create (or create (member "--create" opts-args :test #'string=))
                                        :create-force (or create-force (member "--force-create" opts-args :test #'string=))
                                        :detach (or detach detach-long)
                                        :orphan orphan
                                        :no-guess no-guess)
         (error (e)
           (format t "~A Ошибка: ~A~%"
                   (cl-git-tree/loc:find-emo ws "error")
                   e)))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "switch" #'cmd-switch "Переключиться на ветку (в текущем репозитории)"))
