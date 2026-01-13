;;;; ./src/commands/single-repo/checkout.lisp

(defpackage :cl-git-tree/commands/single-repo/checkout
  (:use :cl)
  (:export cmd-checkout))

(in-package :cl-git-tree/commands/single-repo/checkout)

(defun cmd-checkout (&rest args)
  "CLI-команда: выполнить git checkout в текущем репозитории.
Использование:
  git-tree checkout [опции] TARGET [PATHS...]"
  (cond
    ((member "--help" args :test #'string=)
     (format t "Выполнить git checkout в текущем git-репозитории.~%~%")
     (format t "Использование:~%")
     (format t "  git-tree checkout [опции] TARGET~%")
     (format t "  git-tree checkout [опции] -- PATHS...~%~%")
     (format t "Опции:~%")
     (format t "  -q, --quiet            подавить вывод~%")
     (format t "  -f, --force            принудительный checkout~%")
     (format t "  -m, --merge            трёхсторонее слияние~%")
     (format t "  -b NEW_BRANCH          создать новую ветку~%")
     (format t "  -B NEW_BRANCH          создать/пересоздать ветку~%")
     (format t "  -d, --detach           отсоединённое состояние HEAD~%")
     (format t "  --orphan               создать ветку-сироту~%")
     (format t "  --ours                 разрешить конфликты в нашу пользу~%")
     (format t "  --theirs               разрешить конфликты в их пользу~%")
     (format t "~%Примеры:~%")
     (format t "  git-tree checkout main~%")
     (format t "  git-tree checkout -b feature/new~%")
     (format t "  git-tree checkout -- src/file.lisp~%"))
    ((null args)
     (format t "~A Укажите целевую ветку или коммит~%"
             (cl-git-tree/loc:find-emo (cl-git-tree/loc:make-workspace ".") "error")))
    (t
     (let* ((ws (cl-git-tree/loc:make-workspace "."))
            ;; Найдём разделитель "--"
            (sep-pos (position "--" args :test #'string=))
            (target (if sep-pos nil (first args)))
            (pathspec (if sep-pos (subseq args (1+ sep-pos)) nil))
            (opts-args (if sep-pos (subseq args 0 sep-pos) (rest args)))
            (quiet (or (member "-q" opts-args :test #'string=)
                       (member "--quiet" opts-args :test #'string=)))
            (force (or (member "-f" opts-args :test #'string=)
                       (member "--force" opts-args :test #'string=)))
            (merge (or (member "-m" opts-args :test #'string=)
                       (member "--merge" opts-args :test #'string=)))
            (detach (or (member "-d" opts-args :test #'string=)
                        (member "--detach" opts-args :test #'string=)))
            (orphan (member "--orphan" opts-args :test #'string=))
            (ours (member "--ours" opts-args :test #'string=))
            (theirs (member "--theirs" opts-args :test #'string=)))
       (handler-case
           (cl-git-tree/loc:repo-checkout ws target
                                          :quiet quiet
                                          :force force
                                          :merge merge
                                          :detach detach
                                          :orphan orphan
                                          :pathspec pathspec
                                          :ours ours
                                          :theirs theirs)
         (error (e)
           (format t "~A Ошибка: ~A~%"
                   (cl-git-tree/loc:find-emo ws "error")
                   e)))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "checkout" #'cmd-checkout "Выполнить git checkout (в текущем репозитории)"))
