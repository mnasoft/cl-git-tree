(defpackage :cl-git-tree/commands/remote-create
  (:use :cl)
  (:export cmd-remote-create))

(in-package :cl-git-tree/commands/remote-create)

(defun remote-create-for-workspace (repo-dir args)
  "Вспомогательная функция, вызываемая with-repo для каждого каталога.
ARGS — исходные аргументы команды (первый элемент — ключ локации).
Создаёт workspace и вызывает `repo-create` для найденной в реестре локации."
  (let* ((loc-key (first args))
         (loc (cl-git-tree/loc:find-location loc-key)))
    (unless loc
      (format t "Предупреждение: локация ~A не найдена~%" loc-key)
      (return-from remote-create-for-workspace))
    (let* ((ws (cl-git-tree/loc:make-workspace repo-dir))
           (private (member "--private" args :test #'string=)))
      (if private
          (cl-git-tree/loc:repo-create ws loc :private t)
          (cl-git-tree/loc:repo-create ws loc)))))

(defun cmd-remote-create (&rest args)
  "Создать репозитории для всех локальных git-каталогов по провайдеру.

  Использование (через подкоманду remote):
    git-tree remote create LOC-KEY [--private]

  Алиас: git-tree remote-create LOC-KEY [--private]

  LOC-KEY — ключ локации (как в `~/.git-tree/locations.lisp`).
  Если передан `--private`, в `repo-create` уходит :private t."
  (cond
    ((or (null args) (string= (first args) "--help"))
     (format t "Создать репозитории согласно провайдеру.~%~%")
     (format t "Использование:~%  git-tree remote create LOC-KEY [--private]~%")
     (format t "Алиас:~%  git-tree remote-create LOC-KEY [--private]~%"))
    (t
     (let ((loc-key (first args)))
       (if (null (cl-git-tree/loc:find-location loc-key))
           (format t "Предупреждение: локация ~A не найдена.~%" loc-key)
           (cl-git-tree/fs:with-repo #'remote-create-for-workspace args))))))


