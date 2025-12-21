(in-package :cl-git-tree/loc)

(defmethod days-since-last-commit ((ws <workspace>))
  "Возвращает количество дней с момента последнего коммита или NIL, если дату получить не удалось."
  (let ((last-commit-ts (repo-last-commit-date ws)))
    (when last-commit-ts
      (let* ((now (get-universal-time))
             ;; Разница между Unix epoch (1970-01-01) и Common Lisp epoch (1900-01-01)
             (unix-epoch 2208988800)
             (now-unix (- now unix-epoch))
             (diff-seconds (- now-unix last-commit-ts)))
        (floor diff-seconds 86400)))))

