(in-package :cl-git-tree/loc)

(defmethod repo-status ((ws <workspace>) provider &key &allow-other-keys)
  "Вернуть статус git‑репозитория в рабочем пространстве."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run (<workspace>-path ws) "status" "--short")
    (declare (ignore err))
    (if (= code 0)
        out
        (format nil "Ошибка git status (код ~A)" code))))
