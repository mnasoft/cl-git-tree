(in-package :cl-git-tree/loc)

(defmethod repo-name ((ws <workspace>))
  "Вернуть базовое имя git-репозитория для рабочего пространства."
  (let ((root (git-root ws)))
    (when root
      (car (last (pathname-directory root))))))
