(in-package :cl-git-tree/loc)

(defmethod repo-branches ((ws <workspace>))
  "Вернуть список веток git‑репозитория в рабочем пространстве."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run (<workspace>-path ws) "branch" "--list")
    (declare (ignore err))
    (if (= code 0)
        (split-sequence:split-sequence #\Newline out :remove-empty-subseqs t)
        (list (format nil "Ошибка git branch (код ~A)" code)))))
