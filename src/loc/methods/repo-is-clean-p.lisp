(in-package :cl-git-tree/loc)

(defmethod repo-is-clean-p ((ws <workspace>))
  "Проверяет, что git-репозиторий в рабочем пространстве WS чист (нет незакоммиченных изменений)."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run (<workspace>-path ws) "status" "--short")
    (declare (ignore err))
    (and (zerop code)
         (string= out ""))))
