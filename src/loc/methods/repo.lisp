(in-package :cl-git-tree/loc)


(defmethod repo-status ((ws <workspace>) provider &key &allow-other-keys)
  "Вернуть статус git‑репозитория в рабочем пространстве."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run (<workspace>-path ws) "status" "--short")
    (declare (ignore err))
    (if (= code 0)
        out
        (format nil "Ошибка git status (код ~A)" code))))

(defmethod repo-commit ((ws <workspace>) message)
  "Выполнить коммит в рабочем пространстве с сообщением MESSAGE."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run (<workspace>-path ws) "commit" "-m" message)
    (declare (ignore err))
    (if (= code 0)
        out
        (format nil "Ошибка git commit (код ~A)" code))))

(defmethod repo-branches ((ws <workspace>))
  "Вернуть список веток git‑репозитория в рабочем пространстве."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run (<workspace>-path ws) "branch" "--list")
    (declare (ignore err))
    (if (= code 0)
        (split-sequence:split-sequence #\Newline out :remove-empty-subseqs t)
        (list (format nil "Ошибка git branch (код ~A)" code)))))

(defmethod repo-name ((ws <workspace>))
  "Вернуть базовое имя git-репозитория для рабочего пространства."
  (let ((root (git-root ws)))
    (when root
      (car (last (pathname-directory root))))))

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
    (break "args: ~A" args)
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
