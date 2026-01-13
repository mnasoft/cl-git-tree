;;;; ./src/commands/single-repo/ls-remote.lisp

(defpackage :cl-git-tree/commands/single-repo/ls-remote
  (:use :cl)
  (:export cmd-ls-remote))

(in-package :cl-git-tree/commands/single-repo/ls-remote)

(defun cmd-ls-remote (&rest args)
  "CLI-команда: получить список ссылок из удалённого репозитория.
Использование:
  git-tree ls-remote [опции] REPOSITORY [PATTERNS...]"
  (cond
    ((member "--help" args :test #'string=)
     (format t "Получить список ссылок из удалённого репозитория.~%~%")
     (format t "Использование:~%")
     (format t "  git-tree ls-remote [опции] REPOSITORY [PATTERNS...]~%~%")
     (format t "Опции:~%")
     (format t "  --branches             показать только ветки~%")
     (format t "  --tags                 показать только теги~%")
     (format t "  --refs                 не показывать peeled tags~%")
     (format t "  -q, --quiet            не выводить URL~%")
     (format t "  --exit-code            выйти с кодом 2 если ничего не найдено~%")
     (format t "  --get-url              показать URL и выйти~%")
     (format t "  --sort=<key>           сортировать результаты~%")
     (format t "  --symref               показать символьные ссылки~%")
     (format t "~%Примеры:~%")
     (format t "  git-tree ls-remote origin~%")
     (format t "  git-tree ls-remote --branches origin~%")
     (format t "  git-tree ls-remote --tags origin~%"))
    ((null args)
     (format t "~A Укажите репозиторий (URL или имя remote)~%"
             (cl-git-tree/loc:find-emo (cl-git-tree/loc:make-workspace ".") "error")))
    (t
     (let* ((ws (cl-git-tree/loc:make-workspace "."))
            (repository (first args))
            (patterns (rest args))
            (opts-args args)
            (branches (member "--branches" opts-args :test #'string=))
            (tags (member "--tags" opts-args :test #'string=))
            (refs (member "--refs" opts-args :test #'string=))
            (quiet (or (member "-q" opts-args :test #'string=)
                       (member "--quiet" opts-args :test #'string=)))
            (exit-code (member "--exit-code" opts-args :test #'string=))
            (get-url (member "--get-url" opts-args :test #'string=))
            (symref (member "--symref" opts-args :test #'string=))
            ;; Ищем --sort=<key>
            (sort-arg (find-if (lambda (arg)
                                  (cl-ppcre:scan "^--sort=" arg))
                               opts-args))
            (sort-key (when sort-arg
                        (subseq sort-arg 7))))
       (handler-case
           (cl-git-tree/loc:repo-ls-remote ws repository
                                           :branches branches
                                           :tags tags
                                           :refs refs
                                           :quiet quiet
                                           :exit-code exit-code
                                           :get-url get-url
                                           :sort sort-key
                                           :symref symref
                                           :patterns (when patterns patterns))
         (error (e)
           (format t "~A Ошибка: ~A~%"
                   (cl-git-tree/loc:find-emo ws "error")
                   e)))))))

(eval-when (:load-toplevel :execute)
  (cl-git-tree/dispatch:register-command
   "ls-remote" #'cmd-ls-remote "Получить список ссылок из удалённого репозитория"))
