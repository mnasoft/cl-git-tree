(in-package :cl-git-tree/loc)

(defmethod repo-commit ((ws <workspace>) message)
  "Выполнить коммит в рабочем пространстве с сообщением MESSAGE."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run (<workspace>-path ws) "commit" "-m" message)
    ;;(declare (ignore err))
    (if (= code 0)
        out
        (format nil "Ошибка git commit (код ~A)~%~A" code err))))
