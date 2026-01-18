;;;; ./src/commands/remote-remove.lisp
(in-package :cl-git-tree/commands/remote)

(defun remove-remote-from-repo (repo-dir args)
  "Удаляет remote LOC-KEY из одного репозитория REPO-DIR.

LOC-KEY передаётся как первый элемент ARGS."
  (let* ((loc-key (first args))
         (provider (cl-git-tree/loc:find-location loc-key))
         (ws (cl-git-tree/loc:make-workspace repo-dir)))
    (if provider
        (cl-git-tree/loc:remote-remove ws provider)
        (format t "~A  Локация '~A' не найдена в *locations*~%" (cl-git-tree/loc:find-emo ws "warning") loc-key))))

(defun cmd-remote-remove (&rest args)
  "CLI-команда: удалить remote LOC-KEY из одного репозитория.

Пример использования:
  git-tree remote-remove gh
  git-tree remote-remove gh --verbose"
  (cond
    ;; Показать справку
    ((or (null args)
         (member "--help" args :test #'string=))
     (format t "Удаляет remote из одного git-репозитория.~%~%")
     (format t "Использование:~%  git-tree remote-remove LOC-KEY [--verbose]~%")
     (format t "Пример:~%  git-tree remote-remove gh~%"))
    ;; Запуск по дереву
    (t
     (cl-git-tree/fs:with-repo #'remove-remote-from-repo args))))

;; Регистрация перемещена в remote.lisp (используется как подкоманда 'remote remove')



