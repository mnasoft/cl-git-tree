(in-package :cl-git-tree/loc)

(defmethod repo-add ((ws <workspace>)
                     &key files all update force dry-run &allow-other-keys)
  "Добавить файлы в git индекс для рабочего пространства."
  (let* ((root (git-root ws))
         (args '()))
    ;; Добавляем файлы, если указаны
    (when files
      (if (listp files)
          (setf args (append args files))
          (push files args)))
    ;; Добавляем флаги
    (when dry-run  (push "--dry-run" args))
    (when force    (push "--force" args))
    (when update   (push "--update" args))
    (when all      (push "--all" args))
    (push "add" args)
    ;;(break "args: ~A" args)
    ;; Запускаем git add
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
