(in-package :cl-git-tree/loc)

(defmethod repo-last-commit-date ((ws <workspace>))
  "Возвращает дату последнего коммита в формате Unix timestamp или NIL при ошибке."
  (multiple-value-bind (out err code)
      (cl-git-tree/git-utils:git-run (<workspace>-path ws) "log" "-1" "--format=%ct")
    (declare (ignore err))
    (when (zerop code)
      (let ((timestamp-str (string-trim '(#\Space #\Newline #\Return) out)))
        (when (> (length timestamp-str) 0)
          (parse-integer timestamp-str :junk-allowed t))))))
