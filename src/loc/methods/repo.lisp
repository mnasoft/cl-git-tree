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
    ;;(declare (ignore err))
    (if (= code 0)
        out
        (format nil "Ошибка git commit (код ~A)~%~A" code err))))

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
