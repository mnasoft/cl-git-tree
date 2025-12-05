;;;; ./src/loc/methods/repo-add.lisp

(in-package :cl-git-tree/loc)

(defun ensure-list (x)
  "Если X список, вернуть его как есть, иначе обернуть в список."
  (if (listp x)
      x
      (list x)))

(defmethod repo-add ((ws <workspace>)
                     &key files all update force dry-run &allow-other-keys)
  "Добавить файлы в git индекс для рабочего пространства."
  (let* ((root (git-root ws))
         (args (append (list "add")
                       (when dry-run '("--dry-run"))
                       (when force   '("--force"))
                       (when update  '("--update"))
                       (when all     '("--all"))
                       (ensure-list files))))
    (multiple-value-bind (stdout stderr code)
        (apply #'cl-git-tree/git-utils:git-run root args)
      (cond
        ((zerop code)
         (if dry-run
             (format t "📋 Репозиторий ~A: файлы для добавления:~%~A~%"
                     (repo-name ws) stdout)
             (format t "✅ Репозиторий ~A: файлы добавлены в индекс~%"
                     (repo-name ws))))
        (t
         (format t "❌ Ошибка при добавлении файлов в ~A: ~A~%"
                 (repo-name ws) (or stderr stdout)))))
    ws))
