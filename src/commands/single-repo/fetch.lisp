;;;; ./src/commands/single-repo/fetch.lisp

(defpackage :cl-git-tree/commands/single-repo/fetch
  (:use :cl)
  (:export cmd-fetch))

(in-package :cl-git-tree/commands/single-repo/fetch)

(defun cmd-fetch (&rest args)
  "CLI-команда: загрузить объекты и ссылки из удалённого репозитория.
Использование:
  git-tree fetch [опции] [REPOSITORY]"
  (cond
    ((member "--help" args :test #'string=)
     (format t "Загрузить объекты и ссылки из удалённого репозитория.~%~%")
     (format t "Использование:~%")
     (format t "  git-tree fetch [опции] [REPOSITORY]~%~%")
     (format t "Опции:~%")
     (format t "  -q, --quiet            подавить вывод~%")
     (format t "  --all                  загрузить из всех удалённых репозиториев~%")
     (format t "  --multiple             включить несколько репозиториев~%")
     (format t "  --prune                удалить удалённые ссылки~%")
     (format t "  --prune-tags           удалить удалённые теги~%")
     (format t "  --tags                 загрузить все теги~%")
     (format t "  --depth=<глубина>      shallow clone с указанной глубиной~%")
     (format t "  --atomic               использовать атомарные транзакции~%")
     (format t "  --no-recurse-submodules не загружать подмодули~%")
     (format t "  --dry-run              пробный запуск~%")
     (format t "~%Примеры:~%")
     (format t "  git-tree fetch~%")
     (format t "  git-tree fetch --all~%")
     (format t "  git-tree fetch --tags origin~%"))
    ((null args)
     ;; Fetch без аргументов загружает из origin по умолчанию
     (let* ((ws (cl-git-tree/loc:make-workspace "."))
            (quiet (member "-q" args :test #'string=))
            (all (member "--all" args :test #'string=))
            (multiple (member "--multiple" args :test #'string=))
            (prune (member "--prune" args :test #'string=))
            (prune-tags (member "--prune-tags" args :test #'string=))
            (tags (member "--tags" args :test #'string=))
            (atomic (member "--atomic" args :test #'string=))
            (no-recurse-submodules (member "--no-recurse-submodules" args :test #'string=))
            (dry-run (member "--dry-run" args :test #'string=))
            ;; Ищем --depth=<глубина>
            (depth-arg (find-if (lambda (arg)
                                  (cl-ppcre:scan "^--depth=" arg))
                               args))
            (depth (when depth-arg
                     (subseq depth-arg 8))))
       (handler-case
           (cl-git-tree/loc:repo-fetch ws
                                       :all all
                                       :multiple multiple
                                       :prune prune
                                       :prune-tags prune-tags
                                       :tags tags
                                       :depth depth
                                       :atomic atomic
                                       :no-recurse-submodules no-recurse-submodules
                                       :dry-run dry-run
                                       :quiet quiet)
         (error (e)
           (format t "~A Ошибка: ~A~%"
                   (cl-git-tree/loc:find-emo ws "error")
                   e)))))
    (t
     (let* ((ws (cl-git-tree/loc:make-workspace "."))
            (remote (first args))
            (opts-args (rest args))
            (quiet (or (member "-q" opts-args :test #'string=)
                       (member "--quiet" opts-args :test #'string=)))
            (all (member "--all" opts-args :test #'string=))
            (multiple (member "--multiple" opts-args :test #'string=))
            (prune (member "--prune" opts-args :test #'string=))
            (prune-tags (member "--prune-tags" opts-args :test #'string=))
            (tags (member "--tags" opts-args :test #'string=))
            (atomic (member "--atomic" opts-args :test #'string=))
            (no-recurse-submodules (member "--no-recurse-submodules" opts-args :test #'string=))
            (dry-run (member "--dry-run" opts-args :test #'string=))
            ;; Ищем --depth=<глубина>
            (depth-arg (find-if (lambda (arg)
                                  (cl-ppcre:scan "^--depth=" arg))
                               opts-args))
            (depth (when depth-arg
                     (subseq depth-arg 8))))
       (handler-case
           (cl-git-tree/loc:repo-fetch ws
                                       :remote remote
                                       :all all
                                       :multiple multiple
                                       :prune prune
                                       :prune-tags prune-tags
                                       :tags tags
                                       :depth depth
                                       :atomic atomic
                                       :no-recurse-submodules no-recurse-submodules
                                       :dry-run dry-run
                                       :quiet quiet)
         (error (e)
           (format t "~A Ошибка: ~A~%"
                   (cl-git-tree/loc:find-emo ws "error")
                   e)))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "fetch" #'cmd-fetch "Загрузить объекты и ссылки из удалённого репозитория"))
